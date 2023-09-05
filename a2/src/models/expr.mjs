class Expr {
    expr;
    basic;

    constructor(expr, basic) {
        this.expr = expr;
        this.basic = basic;
    }

    static createBasic(basic) {
        return new Expr(undefined, basic);
    }
    static createExpression(expr, basic) {
        return new Expr(expr, basic);
    }
}

class Basic {
    assignment;
    value;

    constructor(assignment, value) {
        this.assignment = assignment;
        this.value = value;
    }

    static createAssignment(assignment) {
        return new Basic(assignment);
    }
    static createValue(value) {
        return new Basic(undefined, value);
    }
}

class Value {
    number;
    name;
    functionCall;
    functionDefinition;

    constructor(number, name, functionCall, functionDefinition) {
        this.number = number;
        this.name = name;
        this.functionCall = functionCall;
        this.functionDefinition = functionDefinition;
    }

    static createNumber(number) {
        const parsed = parseInt(number, 10);
        return new Value(parsed);
    }

    static createName(name) {
        return new Value(undefined, name);
    }
    static createFunctionCall(functionCall) {
        return new Value(undefined, undefined, functionCall);
    }
    static createFunctionDefinition(functionDefinition) {
        return new Value(undefined, undefined, undefined, functionDefinition);
    }
}

class FunctionCall {
    name;
    values;

    constructor(name, values) {
        this.name = name;
        this.values = values;
    }
}

class FunctionDefinition {
    names;
    expressions;

    constructor(names, expressions) {
        this.names = names;
        this.expressions = expressions;
    }
}


class Assign {
    name;
    expr;

    constructor(name, expr) {
        this.name = name;
        this.expr = expr;
    }
}

export { Expr, Basic, Value, FunctionCall, FunctionDefinition, Assign };
