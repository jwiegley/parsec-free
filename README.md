Welcome to `parsec-free`, a deep-embedding of the Parsec API, to allow for
comprehensive debugging and analysis of Parsec-style parsers.

To use it, simply replace your dependency on `parsec` with `parsec-free`.
None of your code should need to change.  Then, use `parseTestLog` rather than
`parseTest`, and you should see output like the following:

```
label "object name"
  reserved "object"
  identifier => "ActUp"
parserPlus
  (reserved "identity")
  return
parserPlus
  reserved "state"
  label "variable declarations"
    many1
      label "type"
        choice
          reserved "int"
      commaSep1
        sepBy1
          label "variable name"
            identifier => "bar"
          (string ",")
      reservedOp ";"
      (label "type")
        (choice)
          (reserved "int")
          (reserved "pointer")
          (label "user type name")
            (identifier)
parserPlus
  reserved "events"
  many1
    label "event definition kind"
      choice
        (reserved "internal")
        reserved "imported"
    label "error keyword"
      parserPlus
        (reserved "error")
        return
...
```

In this output, terms in parentheses represent a failed attempt to parse along
that branch. If a term ends with `...`, it means the parser did not complete
before an exception occurred.

Also, `parserPlus` is the name for `<|>`, while `label` is `<?>`.
