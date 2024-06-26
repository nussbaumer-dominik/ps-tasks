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
    list
    functionCall;
    functionDefinition;

    constructor(number, name, list, functionCall, functionDefinition) {
        this.number = number;
        this.name = name;
        this.list = list;
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

    static createList(list) {
        return new Value(undefined, undefined, list);
    }

    static createFunctionCall(functionCall) {
        return new Value(undefined, undefined, undefined, functionCall);
    }

    static createFunctionDefinition(functionDefinition) {
        return new Value(undefined, undefined, undefined, undefined, functionDefinition);
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

export {Basic, Value, FunctionCall, FunctionDefinition, Assign};
