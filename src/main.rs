use std::iter::FromIterator;
use std::path::PathBuf;
use std::process::ExitCode;
use std::{collections::HashSet, path::Path};

use cargo_metadata::{CargoOpt, MetadataCommand, Package, PackageId, Resolve, TargetKind};

use clap::Parser;
use error::Error;
use regex::Regex;
use tracing::{debug, error, info};
mod error;
mod manifest;
mod unfeature;

/// Generate new crate with inlined enabled/disabled features
#[derive(Debug, Parser)]
#[clap(version, about)]
struct Args {
    /// Has to be "unfeature"
    command: String,
    /// Output directory
    output: PathBuf,
    /// Regex to match features
    match_features: Regex,
    /// Additional files to copy
    additional_files: Vec<String>,
    /// Enabled features
    #[clap(short = 'F', long)]
    features: Vec<String>,
    /// Disable default features
    #[clap(long)]
    no_default_features: bool,
    /// Enable all features
    #[clap(long)]
    all_features: bool,
    /// Path to Cargo.toml
    #[clap(long)]
    manifest_path: Option<PathBuf>,
}

fn main() -> ExitCode {
    let Args {
        command,
        output,
        match_features,
        additional_files,
        features,
        no_default_features,
        all_features,
        manifest_path,
    } = Args::parse();

    tracing_subscriber::fmt()
        .compact()
        .with_target(false)
        .with_file(true)
        .with_line_number(true)
        .without_time()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    if command != "unfeature" {
        error!("Invalid command: {command}");
        error!("This tool is expected to be executed by cargo: cargo unfeature");
        return ExitCode::FAILURE;
    }

    info!("match: {match_features:?}");

    let mut meta_cmd = MetadataCommand::new();
    if let Some(manifest_path) = manifest_path {
        meta_cmd.manifest_path(manifest_path);
    }
    if all_features {
        meta_cmd.features(CargoOpt::AllFeatures);
    }
    if no_default_features {
        meta_cmd.features(CargoOpt::NoDefaultFeatures);
    }
    if !features.is_empty() {
        meta_cmd.features(CargoOpt::SomeFeatures(features));
    }
    let meta = meta_cmd.exec().expect("Failed to get metadata");

    info!("{:?}", meta.workspace_default_members);

    let resolve = meta.resolve.expect("Failed to get resolve");
    info!("{:?}", resolve.root);
    let root_id = resolve.root.clone().expect("Failed to get root node");

    let root = meta.workspace_root.as_std_path();

    unfeature_crate(
        root,
        &output,
        root_id,
        &resolve,
        &meta.packages,
        &match_features,
    )
    .unwrap();

    for file in additional_files {
        let (src, dst) = if let Some(parts) = file.split_once('=') {
            parts
        } else {
            (&*file, &*file)
        };

        let file = Path::new(src).canonicalize().unwrap();
        let dest_path = output.join(dst);
        info!(
            "Copy: {:?} -> {dest_path:?}",
            file.strip_prefix(root).unwrap()
        );
        std::fs::create_dir_all(dest_path.parent().unwrap()).unwrap();
        std::fs::copy(&file, dest_path).unwrap();
    }

    ExitCode::SUCCESS
}

fn unfeature_crate(
    root: &Path,
    destination: &Path,
    id: PackageId,
    resolve: &Resolve,
    packages: &[Package],
    match_features: &Regex,
) -> Result<(), Error> {
    let node = resolve
        .nodes
        .iter()
        .find(|node| node.id == id)
        .expect("Failed to find node");
    let package = packages
        .iter()
        .find(|pkg| pkg.id == id)
        .expect("Failed to find package");
    if package.source.is_some() {
        return Ok(());
    }

    let enabled_features =
        HashSet::<String>::from_iter(node.features.iter().map(|f| f.to_string()));

    for target in &package.targets {
        if !target
            .kind
            .iter()
            .any(|k| *k == TargetKind::Bin || *k == TargetKind::Lib)
        {
            continue;
        }

        let src_path = root.join(&target.src_path);
        unfeature_module(
            root,
            &src_path,
            destination,
            match_features,
            &enabled_features,
        )?;
    }

    for dep in &node.deps {
        unfeature_crate(
            root,
            destination,
            dep.pkg.clone(),
            resolve,
            packages,
            match_features,
        )?;
    }

    // Copy and handle manifest
    manifest::unfeature(
        root,
        package.manifest_path.as_std_path(),
        destination,
        match_features,
        &enabled_features,
    )?;

    let manifest_dir = package.manifest_path.parent().unwrap();
    let additional_files = [
        "README.md",
        "LICENSE",
        "build.rs",
        "rust-toolchain",
        "rust-toolchain.toml",
        "rustfmt.toml",
        ".gitignore",
        ".cargo/config.toml",
        ".cargo/config",
    ];
    for file in additional_files.iter() {
        let file_path = manifest_dir.join(file);
        if file_path.exists() {
            if file_path.extension() == Some("rs") {
                unfeature_module(
                    root,
                    file_path.as_std_path(),
                    destination,
                    match_features,
                    &enabled_features,
                )?;
            } else {
                let dest_path = destination.join(file_path.strip_prefix(root).unwrap());
                info!(
                    "Copy: {:?} -> {dest_path:?}",
                    file_path.strip_prefix(root).unwrap()
                );
                std::fs::create_dir_all(dest_path.parent().unwrap())?;
                std::fs::copy(&file_path, dest_path)?;
            }
        }
    }

    Ok(())
}

fn unfeature_module(
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
    let code = std::fs::read_to_string(src_path)?;
    let (code, submodules) = unfeature::unfeature(&code, match_features, enabled_features)?;
    std::fs::create_dir_all(destination.parent().unwrap())?;
    std::fs::write(&destination, code)?;
    debug!("Submodules: {submodules:?}");

    for submodule in submodules {
        if submodule.contains('.') {
            debug!("Included file: {submodule:?}");
            let file_path = src_path.parent().unwrap().join(submodule).canonicalize()?;
            let dest_path = dest_dir.join(file_path.strip_prefix(root).unwrap());
            info!(
                "Copy: {:?} -> {dest_path:?}",
                file_path.strip_prefix(root).unwrap()
            );
            std::fs::create_dir_all(dest_path.parent().unwrap())?;
            std::fs::copy(&file_path, dest_path)?;
            continue;
        }
        if submodule.ends_with("/") {
            debug!("Skipping directory: {submodule:?}");
            continue; // Skip directories
        }

        let mut file_path = src_path.with_file_name(&submodule).with_extension("rs");
        if !file_path.exists() {
            file_path = src_path.with_file_name(&submodule).join("mod.rs");
        }
        unfeature_module(root, &file_path, dest_dir, match_features, enabled_features)?;
    }

    Ok(())
}
