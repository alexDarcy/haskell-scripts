# Stages
Group the number of similar entries and count them. It also removes irrelevant
data (in my use case) from the export (pdf->text).

The input must be a .txt file given py `pdftotext -layout`.

The output is a .csv file (for comparison) and a nice latex file (to be compiled
with `lualatex`).


## Running test
`stack test`

## Creating a test case
Remove useless lines (empty lines and so on).
Only keep the column with the name of the internship (2nd column)
Then sort it with case *sensitive*. The default `sort` is case insensitive so
you have to do `LC_COLLATE=C sort` on it.
Then count entries with `uniq -c`.
