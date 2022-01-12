type variable = string

type expr = StringLiteral of string

type statement = FunctionCall of variable * expr list

type program = {name: string; main: statement list}
