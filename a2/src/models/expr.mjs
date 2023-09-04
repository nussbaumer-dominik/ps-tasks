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

    constructor(number, name, functionCall) {
        this.number = number;
        this.name = name;
        this.functionCall = functionCall;
    }

    static createNumber(number) {
        return new Value(number);
    }

    static createName(name) {
        return new Value(undefined, name);
    }
    static createFunctionCall(functionCall) {
        return new Value(undefined, undefined, functionCall);
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

class Assign {
    name;
    expr;

    constructor(name, expr) {
        this.name = name;
        this.expr = expr;
    }
}

export { Expr, Basic, Value, FunctionCall, Assign };
