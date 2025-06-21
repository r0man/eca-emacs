# ECA Emacs

:warning: The project is still alpha and experimental, with bugs and missing features, but being consistenly improved.

[![MELPA](https://melpa.org/packages/eca-emacs-badge.svg)](https://melpa.org/#/eca-emacs)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](./LICENSE)

![demo](./demo.png)

ECA (Editor Code Assistant) Emacs is an AI-powered pair-programming client for Emacs.
Inspired by lsp-mode’s JSONRPC handling, it connects to an external `eca` server process to provide interactive chat, code suggestions, context management and more.

For more details about ECA, check [ECA server](https://github.com/editor-code-assistant/eca).

## Requirements

- Emacs 28.1 or later
- External `eca` server binary
  - Automatic downloaded if `eca-custom-command` is `nil`
  - Place it on your `$PATH` or customize `eca-custom-command`

## Installation

### Melpa (wip)

```
M-x package-install eca
```

### Doom Emacs:

```elisp
(package! eca :recipe (:fetcher github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
```

## Quickstart

1. Run `M-x eca` to start the eca process and initialize the workspace.
2. The dedicated chat window `<eca-chat>` pops up.
3. Type your prompt after the `> ` and press RET.
4. Attach more context auto completing after the `@`.

## Contributing

Contributions are very welcome, please open a issue for discussion or pull request.
