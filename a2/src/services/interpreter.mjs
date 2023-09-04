import {Basic, Value} from "../models/expr.mjs";

const memory = new Map()

function findResult(name) {
    if(!memory.has(name)){
        return undefined;
    }
    const entry = memory.get(name);
    if(entry.value) {
        return entry.value;
    }
    return interpretExpression(entry.expression);
}

function interpretExpression(expression) {
    let value, assignment = undefined;
    if (expression instanceof Basic) {
        value = expression.value;
        assignment = expression.assignment;
    }
    if(expression instanceof Value) {
        value = expression;
    }


    if (assignment) {
        memory.set(assignment.name, {
            expression: assignment.expr,
            value: undefined,
        })
    }
    if (value) {
        if (value.number) {
            return value.number
        }
        if (value.name) {
            return findResult(value.name)
        }
        if (value.functionCall) {
            const result = findResult(value.functionCall.name)
            if (result === undefined) {
                const builtin = builtins[value.functionCall.name];
                if (builtin) {
                    const callValues = value.functionCall.values.map(interpretExpression)
                    return builtin(callValues)
                } else {
                    throw new Error(`Function ${value.functionCall.name} not found`)
                }
            } else {
                return result
            }
        }
    }
}

function interpret(expressions) {
    for (let expression of expressions) {
        console.log(interpretExpression(expression));
    }
}

const builtins = {
    "print": (value) => console.log(value),
    "plus": (values) => values[0] + values[1],
    "mult": (values) => values[0] * values[1],
}

export default interpret;