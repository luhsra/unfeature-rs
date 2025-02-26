use std::collections::HashSet;
use std::fmt::Debug;
use std::panic::Location;

use log::{debug, trace};
use proc_macro2::{Span, TokenTree};
use quote::ToTokens;
use regex::Regex;
use syn::buffer::Cursor;
use syn::meta::ParseNestedMeta;
use syn::parse::StepCursor;
use syn::spanned::Spanned;
use syn::visit::Visit;
use syn::{parenthesized, AttrStyle, Attribute, LitStr};
use syn::{token, Macro};

use crate::error::Error;

pub fn unfeature(
    input: &str,
    known_features: &Regex,
    enabled_features: &HashSet<String>,
) -> Result<(String, Vec<String>), Error> {
    let mut parsed = syn::parse_file(&input)?;

    let mut remover = FeatureRemover::new(&known_features, &enabled_features);
    remover.visit_file(&mut parsed);

    let mut output = input.to_string();

    let mut offset = 0;
    let mut last_end = 0;
    for (span, replacement) in remover.removed {
        debug!("removing: {}:{}", span.start().line, span.start().column);
        let range = span.byte_range();

        if range.start < last_end {
            debug!("overlapping: {}:{}", span.start().line, span.start().column);
            continue;
        }

        let range = if replacement.is_empty() {
            // Increase range to include prefixed and trailing space
            fn no_space(c: char) -> bool {
                c != ' ' && c != '\t'
            }
            let line_start = input[..range.start]
                .rfind(no_space)
                .map(|i| i + 1)
                .unwrap_or(0);
            let line_end = input[range.end..]
                .find(no_space)
                .map(|i| range.end + i + 1)
                .unwrap_or(input.len());
            line_start..line_end
        } else {
            range
        };
        let range = range.start - offset..range.end - offset;

        output.replace_range(range.clone(), &replacement);
        offset += range.end - range.start;
        offset -= replacement.len();

        if replacement.is_empty() {
            // remove two consecutive newlines
            if let Some(this_ln) = output[..range.start].rfind('\n') {
                let previous_ln = output[..this_ln].rfind('\n').map(|i| i + 1).unwrap_or(0);
                if output[previous_ln..this_ln].trim().is_empty() {
                    if let Some(next_ln) = output[range.start..].find('\n') {
                        if output[range.start..range.start + next_ln].trim().is_empty() {
                            debug!("empty lines: {}:{}", span.start().line, span.start().column);
                            output.replace_range(previous_ln..next_ln + range.start, "");
                            offset += (next_ln + range.start) - previous_ln;
                        }
                    }
                }
            } else {
                // Remove empty line at the start of the file
                output.replace_range(0..range.start, "");
                offset += range.start;
            }
        }

        last_end = span.byte_range().end;
    }

    output = format!("{}\n", output.trim());

    Ok((output, remover.submodules))
}

struct FeatureRemover<'a> {
    known_features: &'a Regex,
    enabled_features: &'a HashSet<String>,
    removed: Vec<(Span, String)>,
    parents: Vec<Span>,
    submodules: Vec<String>,
    inside_attr: bool,
}

impl<'a, 'ast> FeatureRemover<'a> {
    fn new(known_features: &'a Regex, enabled_features: &'a HashSet<String>) -> Self {
        Self {
            known_features,
            enabled_features,
            removed: Vec::new(),
            parents: Vec::new(),
            submodules: Vec::new(),
            inside_attr: false,
        }
    }

    #[track_caller]
    fn visit_attributed<T: Spanned + Debug>(
        &mut self,
        spanned: &'ast T,
        visit_inner: impl FnOnce(&mut Self, &'ast T),
    ) {
        if self.inside_attr {
            visit_inner(self, spanned);
        } else {
            trace!(
                "visit: {}:{} {} {spanned:?}",
                spanned.span().start().line,
                spanned.span().start().column,
                Location::caller()
            );
            self.parents.push(spanned.span());
            // TODO: Visit only if span is not removed
            visit_inner(self, spanned);
            self.parents.push(spanned.span());
        }
    }

    fn parse_cfg(attr: &Attribute) -> Option<Config> {
        let mut config = None;
        if attr.path().is_ident("cfg") {
            let e = attr.parse_nested_meta(|meta| {
                config = Some(Config::parse_nested(meta)?);
                Ok(())
            });
            if let Err(e) = e {
                debug!("skip: {:?} {}", attr.to_token_stream().to_string(), e);
            }
        }
        config
    }
    fn parse_cfg_attr(attr: &Attribute) -> Option<(Config, Vec<String>)> {
        if attr.path().is_ident("cfg_attr") {
            let mut config = None;
            let mut replacements = Vec::new();

            let e = attr.parse_nested_meta(|meta| {
                let name = meta.path.to_token_stream().to_string();
                if config.is_none() {
                    // cfg attribute at the beginning
                    config = Some(Config::parse_nested(meta)?);
                } else {
                    // Following attributes
                    let replacement = if let Ok(value) = meta.value() {
                        let inner = value.span().source_text().unwrap();
                        value.step(read_to_end).unwrap();
                        format!("{name} = {inner}")
                    } else if meta.input.peek(token::Paren) {
                        let content;
                        parenthesized!(content in meta.input);
                        let inner = content.span().source_text().unwrap();
                        content.step(read_to_end).unwrap();
                        format!("{name}({inner})")
                    } else {
                        format!("{name}")
                    };
                    let replacement = match attr.style {
                        AttrStyle::Outer => format!("#[{replacement}]"),
                        AttrStyle::Inner(_) => format!("#![{replacement}]"),
                    };
                    replacements.push(replacement);
                }
                Ok(())
            });
            if let Err(e) = e {
                debug!("skip: {:?} {}", attr.to_token_stream().to_string(), e);
            } else {
                return config.map(|c| (c, replacements));
            }
        }
        None
    }
}

impl<'a, 'ast> Visit<'ast> for FeatureRemover<'a> {
    fn visit_attribute(&mut self, attr: &'ast Attribute) {
        if self.inside_attr {
            return;
        }

        self.inside_attr = true;
        syn::visit::visit_attribute(self, attr);
        self.inside_attr = false;

        if let Some((removed, _)) = self.removed.last() {
            if removed
                .byte_range()
                .contains(&attr.span().byte_range().start)
            {
                debug!("skip: {}", attr.to_token_stream().to_string());
                return;
            }
        }

        if let Some(config) = Self::parse_cfg(attr) {
            if let Some(enabled) = config.enabled(&self.known_features, &self.enabled_features) {
                debug!("enabled: {} {enabled}", attr.to_token_stream().to_string());

                if enabled {
                    self.removed.push((attr.span(), String::new()));
                } else {
                    trace!(
                        "remove: {}: {:?} -- {:?}",
                        attr.to_token_stream().to_string(),
                        self.parents.last().unwrap().start(),
                        self.parents.last().unwrap().end()
                    );
                    self.removed
                        .push((self.parents.last().unwrap().clone(), String::new()));
                }
            }
        } else if let Some((config, replacements)) = Self::parse_cfg_attr(attr) {
            if let Some(enabled) = config.enabled(&self.known_features, &self.enabled_features) {
                debug!("enabled: {} {enabled}", attr.to_token_stream().to_string());
                if enabled {
                    self.removed.push((attr.span(), replacements.join("")));
                } else {
                    self.removed.push((attr.span(), String::new()));
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast syn::Stmt) {
        self.visit_attributed(stmt, syn::visit::visit_stmt);
    }
    fn visit_item(&mut self, item: &'ast syn::Item) {
        self.visit_attributed(item, syn::visit::visit_item);
    }
    fn visit_expr(&mut self, expr: &'ast syn::Expr) {
        self.visit_attributed(expr, syn::visit::visit_expr);
    }
    fn visit_file(&mut self, file: &'ast syn::File) {
        self.visit_attributed(file, syn::visit::visit_file);
    }
    fn visit_arm(&mut self, arm: &'ast syn::Arm) {
        self.visit_attributed(arm, syn::visit::visit_arm);
    }
    fn visit_field(&mut self, arm: &'ast syn::Field) {
        self.visit_attributed(arm, syn::visit::visit_field);
    }
    fn visit_member(&mut self, arm: &'ast syn::Member) {
        self.visit_attributed(arm, syn::visit::visit_member);
    }
    fn visit_field_value(&mut self, arm: &'ast syn::FieldValue) {
        self.visit_attributed(arm, syn::visit::visit_field_value);
    }
    fn visit_variant(&mut self, arm: &'ast syn::Variant) {
        self.visit_attributed(arm, syn::visit::visit_variant);
    }
    fn visit_fn_arg(&mut self, i: &'ast syn::FnArg) {
        self.visit_attributed(i, syn::visit::visit_fn_arg);
    }
    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        self.visit_attributed(module, syn::visit::visit_item_mod);

        // We are only looking for external submodules
        if !module.content.is_none() {
            return;
        }
        if module.attrs.iter().all(|attr| {
            Self::parse_cfg(attr).map_or(true, |c| {
                c.enabled(&self.known_features, &self.enabled_features)
                    .unwrap_or(true)
            })
        }) {
            self.submodules.push(module.ident.to_string());
        }
    }
    fn visit_macro(&mut self, mac: &'ast syn::Macro) {
        self.visit_attributed(mac, syn::visit::visit_macro);

        if mac.path.is_ident("include_str") {
            if let Some(lit) = mac.tokens.clone().into_iter().next() {
                if let TokenTree::Literal(lit) = lit {
                    let path = lit.to_string();
                    let path = &path[1..path.len() - 1];
                    self.submodules.push(path.to_string());
                }
            }
        } else if mac.path.is_ident("global_asm")
            || mac.path.is_ident("asm")
            || mac.path.is_ident("naked_asm")
        {
            // This can also call include_str
            if let Ok(body) = mac.parse_body_with(|f: &syn::parse::ParseBuffer<'_>| {
                let mac = f.parse::<Macro>()?;
                f.step(read_to_end).unwrap();
                Ok(mac)
            }) {
                self.visit_macro(&body);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Config {
    Feature(String),
    Not(Box<Config>),
    Any(Vec<Config>),
    All(Vec<Config>),
}
impl Config {
    fn parse_nested(meta: ParseNestedMeta) -> Result<Self, syn::Error> {
        if meta.path.is_ident("feature") {
            let value: LitStr = meta.value()?.parse()?;
            Ok(Config::Feature(value.value()))
        } else if meta.path.is_ident("not") {
            let mut not = Err(syn::Error::new(meta.path.span(), "expected a nested meta"));
            meta.parse_nested_meta(|meta| {
                not = Ok(Config::parse_nested(meta)?);
                Ok(())
            })?;
            not.map(|c| Config::Not(Box::new(c)))
        } else if meta.path.is_ident("any") {
            let mut any = Vec::new();
            meta.parse_nested_meta(|meta| {
                any.push(Config::parse_nested(meta)?);
                Ok(())
            })?;
            Ok(Config::Any(any))
        } else if meta.path.is_ident("all") {
            let mut all = Vec::new();
            meta.parse_nested_meta(|meta| {
                all.push(Config::parse_nested(meta)?);
                Ok(())
            })?;
            Ok(Config::All(all))
        } else {
            Err(syn::Error::new(
                meta.path.span(),
                "expected `feature`, `not`, `any`, or `all`",
            ))
        }
    }
    fn enabled(&self, known: &Regex, enabled: &HashSet<String>) -> Option<bool> {
        match self {
            Config::Feature(f) => known.is_match(f).then(|| enabled.contains(f)),
            Config::Not(config) => config.enabled(known, enabled).map(|b| !b),
            Config::Any(configs) => {
                if configs
                    .iter()
                    .any(|c| c.enabled(known, enabled) == Some(true))
                {
                    Some(true)
                } else if configs
                    .iter()
                    .all(|c| c.enabled(known, enabled) == Some(false))
                {
                    Some(false)
                } else {
                    None
                }
            }
            Config::All(configs) => {
                if configs
                    .iter()
                    .any(|c| c.enabled(known, enabled) == Some(false))
                {
                    Some(false)
                } else if configs
                    .iter()
                    .all(|c| c.enabled(known, enabled) == Some(true))
                {
                    Some(true)
                } else {
                    None
                }
            }
        }
    }
}

fn read_to_end<'c>(cursor: StepCursor<'c, '_>) -> Result<((), Cursor<'c>), syn::Error> {
    let mut cursor = *cursor;
    while let Some((_, next)) = cursor.token_tree() {
        cursor = next;
    }
    Ok(((), cursor))
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use std::iter::FromIterator;

    use regex::Regex;
    use syn::{parse_quote, spanned::Spanned, Attribute};

    use super::{unfeature, Config};

    #[test]
    fn parse_cfg() {
        fn p_cfg(attr: &Attribute) -> Result<Config, syn::Error> {
            let mut config = None;
            if attr.path().is_ident("cfg") {
                let _ = attr.parse_nested_meta(|meta| {
                    config = Some(Config::parse_nested(meta)?);
                    Ok(())
                });
            }
            config.ok_or_else(|| syn::Error::new(attr.span(), "expected `cfg`"))
        }

        let input: Attribute = parse_quote!(#[cfg(feature = "foo")]);
        assert_eq!(p_cfg(&input).unwrap(), Config::Feature("foo".to_string()));

        let input: Attribute = parse_quote!(#[cfg(not(feature = "foo"))]);
        assert_eq!(
            p_cfg(&input).unwrap(),
            Config::Not(Box::new(Config::Feature("foo".to_string())))
        );

        let input: Attribute = parse_quote!(#[cfg(any(feature = "foo", feature = "bar"))]);
        assert_eq!(
            p_cfg(&input).unwrap(),
            Config::Any(vec![
                Config::Feature("foo".to_string()),
                Config::Feature("bar".to_string())
            ])
        );

        let input: Attribute = parse_quote!(#[cfg(all(feature = "foo", feature = "bar"))]);
        assert_eq!(
            p_cfg(&input).unwrap(),
            Config::All(vec![
                Config::Feature("foo".to_string()),
                Config::Feature("bar".to_string())
            ])
        );

        let input: Attribute =
            parse_quote!(#[cfg(all(feature = "foo", any(feature = "bar", not(feature = "baz"))))]);
        assert_eq!(
            p_cfg(&input).unwrap(),
            Config::All(vec![
                Config::Feature("foo".to_string()),
                Config::Any(vec![
                    Config::Feature("bar".to_string()),
                    Config::Not(Box::new(Config::Feature("baz".to_string()))),
                ])
            ])
        );
    }

    #[test]
    fn basic() {
        let input = r#"
        // Comment
        #[cfg(feature = "foo")]
        fn foo() {}

        #[cfg(not(feature = "foo"))]
        fn not_foo() {}
        #[cfg(not(feature = "foo"))]
        fn not_foo() {}

        #[cfg(any(feature = "foo", feature = "bar"))]
        fn any_foo_bar() {}

        #[cfg(all(feature = "foo", feature = "bar"))]
        fn all_foo_bar() {}

        #[cfg(all(feature = "foo", any(feature = "bar", feature = "baz")))]
        fn all_foo_any_bar_baz() {}
        "#;

        let known_features = Regex::new("(foo|bar|baz)").unwrap();
        let enabled_features = HashSet::from_iter(["foo".to_string(), "baz".to_string()]);

        let (output, _) = unfeature(&input, &known_features, &enabled_features).unwrap();
        println!("Output: {}", output);

        let expected = r#"// Comment
        fn foo() {}

        fn any_foo_bar() {}

        fn all_foo_any_bar_baz() {}
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn remove_whole_file() {
        let input = r#"
        #![cfg(not(feature = "foo"))]

        #[cfg(feature = "bar")]
        fn not_foo() {}
        "#;

        let known_features = Regex::new("(foo|bar)").unwrap();
        let enabled_features = HashSet::from_iter(["foo".to_string()]);

        let (output, _) = unfeature(&input, &known_features.clone(), &enabled_features).unwrap();
        println!("Output: {:?}", output);
        assert_eq!(output.trim(), "");

        let input = "#![cfg(not(feature = \"foo\"))]";
        let (output, _) = unfeature(&input, &known_features, &enabled_features).unwrap();
        println!("Output: {:?}", output);
        assert_eq!(output.trim(), "");
    }

    #[test]
    fn comments() {
        let input = r#"
        #[cfg(feature = "foo")]
        fn foo() {}

        #[cfg(not(feature = "foo"))]
        fn not_foo() {}

        // Keep this comment
        #[cfg(feature = "bar")]
        // Removed comment
        fn bar() {}

        #[cfg(not(feature = "bar"))]
        fn not_bar() {}
        "#;

        let known_features = Regex::new("(foo|bar)").unwrap();
        let enabled_features = HashSet::from_iter(["foo".to_string()]);

        let (output, _) = unfeature(&input, &known_features, &enabled_features).unwrap();
        println!("Output: {:?}", output);

        let expected = r#"fn foo() {}

        // Keep this comment

        fn not_bar() {}
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn cfg_attr() {
        let input = r#"
        #![cfg_attr(feature = "foo", allow(dead_code))]
        #![cfg_attr(feature = "bar", no_std)]
        #![cfg_attr(feature = "baz", doc = "Some docs")]

        /// Some docs
        #[cfg_attr(feature = "foo", allow(dead_code))]
        fn foo() {}

        /// Also docs
        #[cfg_attr(not(feature = "foo"), allow(dead_code))]
        fn not_foo() {}

        #[cfg_attr(any(feature = "foo", feature = "bar"), allow(dead_code))]
        fn any_foo_bar() {}

        #[cfg_attr(all(feature = "foo", feature = "bar"), allow(dead_code))]
        fn all_foo_bar() {}

        #[cfg_attr(all(feature = "foo", any(feature = "bar", feature = "baz")), allow(dead_code))]
        fn all_foo_any_bar_baz() {}
        "#;

        let known_features = Regex::new("(foo|bar|baz)").unwrap();
        let enabled_features = HashSet::from_iter(["foo".to_string(), "baz".to_string()]);

        let (output, _) = unfeature(&input, &known_features, &enabled_features).unwrap();
        println!("Output: {}", output);
        let expected = r#"#![allow(dead_code)]
        #![doc = "Some docs"]

        /// Some docs
        #[allow(dead_code)]
        fn foo() {}

        /// Also docs
        fn not_foo() {}

        #[allow(dead_code)]
        fn any_foo_bar() {}

        fn all_foo_bar() {}

        #[allow(dead_code)]
        fn all_foo_any_bar_baz() {}
"#;
        assert_eq!(output, expected);
    }
}
