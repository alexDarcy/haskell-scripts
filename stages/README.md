# Stages
Group the number of similar entries and count them. It also removes irrelevant
data (in my use case) from the export (pdf->text).

The input must be a .txt file given py `pdftotex -layout`.

## Running test
`stack test`

## Creating a test case
Remove useless lines (empty lines and so on).
Only keep the column with the name of the internship (2nd column)
