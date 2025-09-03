// Configuration to generate the visitor pattern classes

/**
 * Every root element in the object is the name for a syntax tree node.
 * The sub-elements correspond to the parameters for the node, where
 * the key is the paramater name and the value is the type (Any valid typescript notation).
 * Types beginning with `?` are interpreted as optional (i.e., they can be `undefined`)
 *
 * e.g., adding `Foo {Bar: "Baz", Fizz: "Buzz"}` will generate a new class
 * named Foo with a property Bar of type Baz and an optional property Fizz of type Buzz
 *
 * @type {{[node: string]: {[arg: string]: string}}}
 */
const nodes = {
  Grammar: {
    rules: "Regla[]",
    globalCode: "?{ before: string; after?: string }",
  },
  Regla: {
    id: "string",
    expr: "Opciones",
    alias: "?string",
    start: "?boolean",
  },
  Opciones: {
    exprs: "Union[]",
  },
  ConteoAction: {
    start: "Predicate",
    end: "Predicate",
    simple: "Predicate",
    delimiter: "Opciones",
  },
  Union: {
    exprs: "Node[]",
    action: "?Predicate",
  },
  Predicate: {
    returnType: "string",
    code: "string",
    assertion: "?string",
    params: "?{ [label: string]: string }",
  },
  Pluck: { labeledExpr: "Label", pluck: "?boolean" },
  Label: { annotatedExpr: "Annotated", label: "?string" },
  Annotated: {
    expr: "Node",
    qty: "?(string|Node|ConteoAction)",
    text: "?boolean",
  },
  Assertion: { assertion: "(Annotated|Predicate)" },
  NegAssertion: { assertion: "(Annotated|Predicate)" },
  String: { val: "string", isCase: "?boolean" },
  Clase: { chars: "(string|Rango)[]", isCase: "?boolean" },
  Rango: { bottom: "string", top: "string" },
  Identificador: { id: "string" },
  Punto: {},
  Fin: {},
};

export default nodes;
