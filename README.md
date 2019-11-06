# plan-rss

# Todo

- only process N most recent entries
- md5/sha256 as `<guid>` values

# Changelog

v0.0.4 (2019-11-06)

- `--disable-pre-tag-wrapping` to stop wrapping text inside `<pre>` tags
- remove support for `-m` and `-s` options (use `--image` and `--atom-self-link`
  instead)

v0.0.3 (2019-11-04)

- version not properly taking into account commits since last tag
- travis-ci built binaries are crashing

v0.0.2 (2019-11-03)

- Remove trailing new-line from `<generator>`
- Windows exe missing from releases

v0.0.1 (2019-11-03)

- Initial release

    > plan-rss --help
    Reads a .plan file from stdin, and prints to stdout a feed with all the parsed entries

    Available options:
      -h, --help               print the help text and exit
      -v, --version            print the version and exit
      -t, --title TITLE (Required)                           use TITLE as feed's <title>
      -l, --link LINK (Required)                           use LINK as feed's <link>
      -m, --image IMAGE        use IMAGE as feed's image <url>
      -s, --atom-link-self SELF                           use SELF as feed's atom:link with rel=self
