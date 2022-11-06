# blink-search

<p align="center">
  <br><strong>In the blink of an eye, the search is complete!</strong>
</p>

<br>

blink-search's goal is to become the fastest search framework in Emacs.

blink-search use python multi-thread technology to search and filter candidates, Emacs only render result and do action.

blink-search will provide smooth completion experience without compromise to slow down emacs' performance.

<p align="center">
  <br>blink-search with split window style
</p>
<img src="./images/blink-search.png">

<p align="center">
  <br>blink-search with popup frame style
</p>
<img src="./images/blink-search-posframe.png">

## Installation

1. Install Emacs 28 and above versions
2. Install Python dependencies: `pip3 install epc requests`
3. Install search tools: 
+ [fd](https://github.com/sharkdp/fd)
+ [ripgrep](https://github.com/BurntSushi/ripgrep)
4. Clone or download this repository (path of the folder is the `<path-to-blink-search>` used below).
5. Add following code in your ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-blink-search>")

(require 'blink-search)
```

## Usage
1. Start blink-search: M-x blink-search 
2. Type keyword to search from multiple backend
3. Type keyword that prefix with `#` to search current buffer
4. Type keyword that prefix with `!` to grep current directory
5. Search current symbol: C-u M-x blink-search

| Grep Buffer                                          | Grep Directory |
| :--------:                                       | :----:                                                      |
| <img src="./images/blink-search-grep-buffer.png" width="400"> | <img src="./images/blink-search-grep-directory.png" width="400"> |

## Keymap
| Key      | Command                   | Description                                                                  |
| :---     | :---                      | :---                                                                         |
| C + n    | blink-search-candidate-select-next           | Select next candidate item                                                        |
| C + p  | blink-search-candidate-select-prev           | Select previous candidate item                                                    |
| M + n    | blink-search-backend-select-next           | Select next backend item                                                        |
| M + p  | blink-search-backend-select-prev           | Select previous backend item                                                    |
| M + j    | blink-search-candidate-group-select-next           | Select next candidate group item                                                        |
| M + k  | blink-search-candidate-group-select-prev           | Select previous candidate group item                                                    |
| C + j  | blink-search-parent           | Jump parent for select candidate item                                                    |
| C + m  | blink-search-do           | Do action for select candidate item                                                    |
| M + w  | blink-search-copy           | Copy select candidate item                                                    |
| C + g  | blink-search-quit           | Quit 

* `blink-search-restart-process`: restart blink-search process (only used for development)

## Option
* `blink-search-enable-posframe`: set this option with `t`, blink-search will use `posframe` instead split bottom layout, this feature need you install [posframe](https://github.com/tumashu/posframe) first

## Search Backend

blink-search has completed the following search backend:

* Buffer List: search buffer list
* Common Directory: search your favorite common directory, you can customize option `blink-search-common-directory`
* Recent File: search recent file
* Current Buffer: use `ripgrep` grep current buffer content
* Grep File: use `ripgrep` grep file under project or current directory
* IMenu: search imenu of current buffer
* Find File: use `fd` search file under project or current directory
* EAF Browser History: search history of EAF browser
* Elisp Symbol: search elisp symbol
* Google Suggest: search google suggestions and open in browser

## Report bug

Please use `emacs -q` and load a minimal setup with only blink-search to verify that the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem still exists, please report it [here](https://github.com/manateelazycat/blink-search/issues/new) with `*blink-search*` buffer content, it contains many clues that can help us locate the problem faster.

If you get a segfault error, please use the following way to collect crash information:

1. Install gdb and turn on option `blink-search-enable-debug`
2. Use the command `blink-search-stop-process` to stop the current process
3. Restart blink-search, send issue with `*blink-search*` buffer content when next crash

## Contributor

<a href = "https://github.com/manateelazycat/blink-search/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/blink-search"/>
</a>
