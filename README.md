
# Say Region

Say Region is a simple Emacs package that sends the selected region to the `say` command, allowing you to listen to the text you've selected.

## Installation

Place the `say-region.el` file in a directory that's part of your Emacs `load-path` and add the following line to your Emacs configuration:

```elisp
(require 'say-region)
```

## Usage

Select a region in your Emacs buffer and press `C-c s` to start the `say` process, which will read the text aloud. To kill the running `say` process, press `C-c k`.

## Note

This package relies on the `say` command, which is available only on MacOS. It will not work on other operating systems without modifications.
