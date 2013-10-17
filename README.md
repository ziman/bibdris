# Bibdris
BibTeX database manager

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
$ idris -o bibdris --package lightyear Main.idr
```

### Dependencies
* [lightyear](https://github.com/ziman/lightyear) parsing combinators
