# Make Decks


[![Build Status](https://img.shields.io/github/workflow/status/Lattyware/makedecks/Build%20and%20publish%20to%20GitHub.)](https://github.com/Lattyware/makedecks/actions)
[![License](https://img.shields.io/github/license/Lattyware/manydecks)](LICENSE)

Make Decks is a free, open source deck editor used to make decks for [Massive Decks][md], a comedy party game based on 
Cards against Humanity. It can be used to make custom decks; or to localize, edit, or update existing decks.

**[Make Decks][hosted]**

[hosted]: https://lattyware.github.io/makedecks
[md]: https://github.com/Lattyware/massivedecks

## About

The editor is open source software available under [the AGPLv3 license](LICENSE) and written in [Elm][elm].

[elm]: https://elm-lang.org/

## Use

To use, [the hosted version][hosted] is easiest. You can of course build the elm code and host the resulting HTML, see 
the below section on building for more detail on this.

You can load decks from and saved to `.deck.json5` files, which can then be added as built-in decks to instances of 
Massive Decks.

Please note that this is intended as more a development tool, and the user interface will not be as polished as the 
core game.

### Building

If you want to build the editor yourself, this repository uses git submodules, meaning you will either need to do 
`git clone --recursive` when cloning or run `git submodule update --init` if you already cloned. You can then use 
`npm run build`.

## Contributing

If you have any problems with the editor, please [raise an issue][issue]. If you would like to help develop it, pull
requests are welcome.

[issue]: https://github.com/Lattyware/massivedecks/issues/new

## Credits

### Maintainers

Many Decks is maintained by [Reread Games][reread].

[reread]: https://www.rereadgames.com/
