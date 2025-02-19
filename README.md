# Unfeature: Expand Known Rust Features

Removes all blocks that are disabled by features and the attributes of enabled features.

```
Usage: <input> <output> <known_features> <enabled_features>

<input>: Path to the input file
<output>: Path to the output file
<known_features>: Regex which matches all features that should be visited
<enabled_features>: Comma-separated list of enabled features
```
