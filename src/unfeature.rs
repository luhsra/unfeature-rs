use std::collections::HashSet;

use proc_macro2::Span;
use quote::ToTokens;
use regex::Regex;
use syn::meta::ParseNestedMeta;
use syn::spanned::Spanned;
use syn::visit::Visit;
use syn::{Attribute, LitStr};

pub fn unfeature(
    input: &str,
    known_features: Regex,
    enabled_features: HashSet<String>,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut parsed = syn::parse_file(&input)?;

    let mut remover = FeatureRemover::new(known_features, enabled_features);
    remover.visit_file(&mut parsed);

    let mut output = input.to_string();

    let mut offset = 0;
    let mut last_end = 0;
    for span in remover.removed {
        eprintln!("removing: {:?} {:?}", span.start(), span.end());
        let range = span.byte_range();

        if range.start < last_end {
            eprintln!("overlapping: {:?} ({last_end} <= {range:?})", span.start());
            continue;
        }

        // Increase range to include prefixed and trailing space
        fn no_space(c: char) -> bool {
            c != ' ' && c != '\t'
        }
        let mut line_start = input[..range.start]
            .rfind(no_space)
            .map(|i| i + 1)
            .unwrap_or(0);
        // If the previous line is empty, include it
        if let Some(this_ln) = input[..line_start].rfind('\n') {
            if let Some(previous_ln) = input[..this_ln].rfind('\n') {
                // TODO: also include comments
                if input[previous_ln + 1..this_ln].trim().is_empty() {
                    line_start = previous_ln + 1;
                }
            }
        }
        let line_end = input[range.end..]
            .find(no_space)
            .map(|i| range.end + i + 1)
            .unwrap_or(input.len());

        println!("range: {:?} -> {:?}", range, line_start..line_end);
        let range = line_start..line_end;

        let range = range.start - offset..range.end - offset;

        output.replace_range(range.clone(), "");
        offset += range.end - range.start;
        last_end = span.byte_range().end;
    }

    Ok(output)
}

struct FeatureRemover {
    known_features: Regex,
    enabled_features: HashSet<String>,
    removed: Vec<Span>,
    parents: Vec<Span>,
}

impl<'ast> FeatureRemover {
    fn new(known_features: Regex, enabled_features: HashSet<String>) -> Self {
        Self {
            known_features,
            enabled_features,
            removed: Vec::new(),
            parents: Vec::new(),
        }
    }

    fn visit_attributed<T: Spanned>(
        &mut self,
        spanned: &'ast T,
        visit_inner: impl FnOnce(&mut Self, &'ast T),
    ) {
        self.parents.push(spanned.span());
        visit_inner(self, spanned);
        self.parents.push(spanned.span());
    }
}

impl<'ast> Visit<'ast> for FeatureRemover {
    fn visit_attribute(&mut self, attr: &'ast Attribute) {
        let mut config = None;

        if attr.path().is_ident("cfg") {
            let e = attr.parse_nested_meta(|meta| {
                config = Some(Config::parse_nested(meta)?);
                Ok(())
            });
            if let Err(e) = e {
                eprintln!("skip: {:?} {}", attr.to_token_stream().to_string(), e);
            }
        }

        if let Some(config) = config {
            if let Some(enabled) = config.enabled(&self.known_features, &self.enabled_features) {
                println!(
                    "enabled: {:?} {enabled}",
                    attr.to_token_stream().to_string()
                );

                if enabled {
                    self.removed.push(attr.span());
                } else {
                    self.removed.push(self.parents.last().unwrap().clone());
                }
            }
        }

        syn::visit::visit_attribute(self, attr);
    }

    fn visit_stmt(&mut self, stmt: &'ast syn::Stmt) {
        self.visit_attributed(stmt, syn::visit::visit_stmt);
    }
    fn visit_item(&mut self, item: &'ast syn::Item) {
        self.visit_attributed(item, syn::visit::visit_item);
    }
    fn visit_file(&mut self, file: &'ast syn::File) {
        self.visit_attributed(file, syn::visit::visit_file);
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
    fn enabled(&self, known_features: &Regex, enabled_features: &HashSet<String>) -> Option<bool> {
        match self {
            Config::Feature(f) => known_features
                .is_match(f)
                .then(|| enabled_features.contains(f)),
            Config::Not(config) => config.enabled(known_features, enabled_features).map(|b| !b),
            Config::Any(configs) => {
                if configs
                    .iter()
                    .any(|c| c.enabled(known_features, enabled_features) == Some(true))
                {
                    Some(true)
                } else if configs
                    .iter()
                    .all(|c| c.enabled(known_features, enabled_features) == Some(false))
                {
                    Some(false)
                } else {
                    None
                }
            }
            Config::All(configs) => {
                if configs
                    .iter()
                    .any(|c| c.enabled(known_features, enabled_features) == Some(false))
                {
                    Some(false)
                } else if configs
                    .iter()
                    .all(|c| c.enabled(known_features, enabled_features) == Some(true))
                {
                    Some(true)
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
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
    fn parse_file() {
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

        let known_features = regex::Regex::new("foo|bar|baz").unwrap();
        let mut enabled_features = std::collections::HashSet::new();
        enabled_features.insert("foo".to_string());
        enabled_features.insert("baz".to_string());

        let output = unfeature(&input, known_features, enabled_features).unwrap();
        println!("Output: {}", output);

        let expected = r#"
        // Comment
        #[cfg(feature = "foo")]
        fn foo() {}

        #[cfg(any(feature = "foo", feature = "bar"))]
        fn any_foo_bar() {}

        #[cfg(all(feature = "foo", any(feature = "bar", feature = "baz")))]
        fn all_foo_any_bar_baz() {}
        "#;
        assert_eq!(output, expected);
    }
}
