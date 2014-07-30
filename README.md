Idris starter pack for the [Vindinium AI challenge](http://vindinium.org)

## Build
```sh
$ idris -o vindinium -p lightyear -p text -p lightyear_text src/Main.idr
```

## Usage

```bash
$ vindinium
```

## Dependencies
* [lightyear](https://github.com/ziman/lightyear) parsing combinators
* [text](https://github.com/ziman/text) + `lightyear_text`
