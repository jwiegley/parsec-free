Welcome to `parsec-free`, a deep-embedding of the Parsec API, to allow for
comprehensive debugging and analysis of Parsec-style parsers.

To use it, simply replace your dependency on `parsec` with `parsec-free`.
None of your code should need to change.  Then, use `parseTestLog` rather than
`parseTest`, and you should see output like the following:

```
label "object name" *
  reserved "object" *
  identifier => "aascllwl" *
parserPlus
- (reserved "identity")
+ return
parserPlus *
+ reserved "state" *
  label "var decls" *
    sepEndBy1 *
    + label "var decl" *
        label "type" *
          choice *
          - (reserved "int")
          - (reserved "pointer")
          + label "type name" *
              identifier => "bool" *
        commaSep1 *
        + sepBy1 *
          + label "variable name" *
              identifier => "nlenshiy" *
          - (string ",")
    + symbol ";" *
    - (label "var decl")
        (label "type")
          (choice)
          - (reserved "int")
          - (reserved "pointer")
          - (label "type name")
              (identifier)
parserPlus *
+ reserved "events" *
  sepEndBy1 *
  + label "event defs" *
      label "event definition kind" *
        choice *
        - (reserved "internal")
        + reserved "imported" *
      label "error keyword" *
        parserPlus *
        + reserved "error" *
      commaSep1 *
      + sepBy1 *
        + label "event name" *
            identifier => "swehegge" *
          label "event params" *
            parens *
              between *
              + string "(" *
              + commaSep
                + sepBy
                  - (label "type")
                      (choice)
                      - (reserved "int")
                      - (reserved "pointer")
                      - (label "type name")
                          (identifier)
              + string ")" *
          label "event definiton" *
            parserPlus *
            + reservedOp "=" *
              label "expr" *
                label "buildExpressionParser" *
        - (string ",")
  + symbol ";" *
  - (label "event defs")
      (label "event definition kind")
        (choice)
        - (reserved "internal")
        - (reserved "imported")
        - (reserved "exported")
...
```

Things to note in this output:

- Terms in parentheses represent a failed attempt to parse along that branch.
- If a term ends with `...`, it means the parser did not complete before an
  exception occurred. This makes it easy to follow the chain of parsers "up to
  the error", although there is no such output above.
- Parsers with potentially more than 1 children use bullets to show those
  children. These bullets have the following meaning: `-` if the parser
  failed, `+` if it succeeded, `?` if it was pending completion before an
  exception occurred.
- A parser followed by `*` consumed input. Pay close attention to failed
  parsers that consume input! They will prevent the second branch of `<|>`
  from being attempted, or may indicate a need for `try`.

Also, `parserPlus` is the name for `<|>`, while `label` is `<?>`.
