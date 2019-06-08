# clw-block-braking - A block braking game with gravity written by Common Lisp as Web application

## Usage

```lisp
> (ql:quickload :qlot)
> (qlot:quickload :clw-block-braking)
> (clw-block-braking:start :port <port number>)
```

After starting, you can access to the tetris by `http://localhost:<port number>` using a web browser.


## Installation

This project depends on liblaries that are not registered in the quicklisp repository. So I recommend to use [qlot](https://github.com/fukamachi/qlot) to install them.

```bash
# Under a directory managed by quicklisp
$ git clone https://github.com/eshamster/clw-tetris.git
$ cd clw-tetris
$ ros install qlot # if you haven't installed
$ qlot install
```

## Operation

| Operation | Explanation |
| :------- | :---------- |
| Move mouse  | Move paddle to left or right |
| Left click | Shoot ball / Add gravity to block |
| Wheel down | Make ball speed-down (Make paddle wide) |
| Wheel up   | Make ball speed-up (Make paddle narrow) |

## Screenshots
### Change ball speed

![Demo of changing ball speed](https://user-images.githubusercontent.com/11023733/59144849-02f4f680-8a17-11e9-8e58-b6bb281bdc62.gif)

### Add gravity to block

![Demo of adding gravity](https://user-images.githubusercontent.com/11023733/59144854-07b9aa80-8a17-11e9-96cb-4a8426b1611e.gif)

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2018 eshamster (hamgoostar@gmail.com)

## License

Licensed under the LLGPL License.
