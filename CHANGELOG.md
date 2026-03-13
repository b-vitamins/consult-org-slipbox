# Changelog

## Unreleased

### Added

- Consult commands for indexed node, file, ref, occurrence, backlink,
  forward-link, reflink, and unlinked-reference workflows.
- `consult-org-slipbox-mode` to override `org-slipbox` reader commands with
  Consult-backed implementations.
- Optional `consult-buffer` integration for open slipbox note buffers.
- Embark action maps for node, file, and location candidates.
- ERT coverage for reader, search, location, and `consult-buffer` integration.
- Local verification targets through `Makefile`.
- GitHub Actions CI for compile, test, and checkdoc verification against a
  sibling `org-slipbox` checkout.

### Changed

- README now documents package installation, `consult-buffer` integration, and
  verification commands.
- Switched the package to `org-slipbox`'s public completion, annotation,
  visit, and link-insertion helpers so cross-package integration no longer
  depends on double-hyphen internals.
