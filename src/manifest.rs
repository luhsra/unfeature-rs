use std::collections::{HashMap, HashSet};
use std::path::Path;

use cargo_manifest::Manifest;
use log::info;
use regex::Regex;

use crate::error::Error;

pub fn unfeature(
    root: &Path,
    src_path: &Path,
    dest_dir: &Path,
    match_features: &Regex,
    enabled_features: &HashSet<String>,
) -> Result<(), Error> {
    let destination = dest_dir.join(src_path.strip_prefix(root).unwrap());

    info!(
        "Unfeature: {:?} -> {destination:?}",
        src_path.strip_prefix(root).unwrap()
    );

    let mut manifest = Manifest::from_path(src_path).unwrap();

    let mut enabled_deps = HashMap::new();

    if let Some(features) = &mut manifest.features {
        features.retain(|feature, deps| {
            if feature == "default" {
                deps.retain(|dep| !match_features.is_match(dep) && !enabled_features.contains(dep));
                return true;
            }

            let matched = match_features.is_match(feature);
            // Enable dependencies for unmatched or enabled features
            if !matched || enabled_features.contains(feature) {
                for dep in deps {
                    if let Some(suffix) = dep.strip_prefix("dep:") {
                        // Enabled features are not optional anymore
                        enabled_deps.insert(suffix.to_string(), !matched);
                    }
                }
            }
            !match_features.is_match(feature)
        });
    }

    if let Some(workspace) = &mut manifest.workspace {
        workspace
            .members
            .retain(|member| dest_dir.join(member).exists());
    }

    fn retain_dep(
        name: &str,
        dep: &mut cargo_manifest::Dependency,
        enabled_deps: &HashMap<String, bool>,
    ) -> bool {
        if let cargo_manifest::Dependency::Detailed(dep) = dep {
            if let Some(optional) = &mut dep.optional {
                if !*optional {
                    return true;
                }
                // Check if feature set enables this dependency
                if let Some(opt) = enabled_deps.get(name) {
                    *optional = *opt;
                } else {
                    return false;
                }
            }
        }
        true
    }

    // Remove disabled optional dependencies
    if let Some(dependencies) = &mut manifest.dependencies {
        dependencies.retain(|name, dep| retain_dep(name, dep, &enabled_deps));
        if dependencies.is_empty() {
            manifest.dependencies = None;
        }
    }
    if let Some(dependencies) = &mut manifest.build_dependencies {
        dependencies.retain(|name, dep| retain_dep(name, dep, &enabled_deps));
        if dependencies.is_empty() {
            manifest.build_dependencies = None;
        }
    }
    if let Some(dependencies) = &mut manifest.dev_dependencies {
        dependencies.retain(|name, dep| retain_dep(name, dep, &enabled_deps));
        if dependencies.is_empty() {
            manifest.dev_dependencies = None;
        }
    }

    // Deprecation
    for bin in manifest.bin.iter_mut() {
        bin.edition = None;
    }
    if let Some(lib) = manifest.lib.as_mut() {
        lib.edition = None;
    }

    std::fs::create_dir_all(destination.parent().unwrap())?;
    let out = toml::to_string_pretty(&manifest).unwrap();
    std::fs::write(&destination, out)?;

    Ok(())
}
