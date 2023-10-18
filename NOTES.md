# stevana.github.io

My static web site generator that uses markdown/pandoc source scattered across
multiple repositories.

The output is hosted on Github pages over at
[https://stevana.github.io](https://stevana.github.io).

## Highlighting

Use the following script to get the CSS from `pandoc`:

```bash
#!/bin/sh
style=${1:-pygments}
tmp=
trap 'rm -f "$tmp"' EXIT
tmp=$(mktemp)
echo '$highlighting-css$' > "$tmp"
echo '`test`{.c}' | pandoc --highlight-style=$style --template=$tmp
```

See the following StackOverflow
[post](https://stackoverflow.com/questions/62774695/pandoc-where-are-css-files-for-syntax-highlighting-code)
for details.

## RSS

* https://validator.w3.org/feed/
* Just use atom instead? https://validator.w3.org/feed/docs/atom.html

## CI

* https://blog.marcnuri.com/triggering-github-actions-across-different-repositories
* https://stackoverflow.com/questions/65499626/trigger-github-actions-if-push-in-another-repo
* https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows#repository_dispatch
* https://github.com/serokell/xrefcheck
