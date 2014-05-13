# Bibdris
BibTeX database manager

NOTE: The version in the `string` branch is more efficient because
it uses the `String` type. However, we want to push `Data.Text` so
we use it here although it is not very efficient yet.

## Usage

```bash
$ bibdris db.bib -l     
$ bibdris db.bib -a url
```

Options:
* `-l`: List all items in the database.
* `-a url`: Add the PDF at the URL (TBD).

## Build
```bash
$ idris -o bibdris -p lightyear -p text -p lightyear_text Main.idr
```

### Dependencies
* [lightyear](https://github.com/ziman/lightyear) parsing combinators
* [text](https://github.com/ziman/text) + `lightyear_text`
