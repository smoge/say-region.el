# say-region.el

An Emacs package that sends a selected region of text to the macOS `say` command, which reads the text out loud using the default text-to-speech voice.

-- UPDATE: there is a espeak alternative (works on linux) on the package now --

## Installation

1. Clone the repository or download the `say-region.el` file.
2. Add the following lines to your Emacs configuration (`init.el` or `.emacs`):

```elisp
(add-to-list 'load-path "/path/to/say-region.el")
(require 'say-region)
```

Replace `/path/to/say-region.el` with the actual path to the `say-region.el` file.

## Usage

1. Enable `say-region-mode` in a buffer:

```elisp
M-x say-region-mode
```

2. Select a region of text in the buffer.
3. Press `C-c s` to send the selected region to the `say` command, which reads the text aloud.
4. To stop the speech, press `C-c k` to kill any running say processes.

## Limitations

This package is designed for macOS and uses the macOS-specific `say` command. It is not compatible with other operating systems without modifications.

## Contributing

Feel free to submit pull requests, report bugs, or suggest new features by opening an issue on the GitHub repository.
