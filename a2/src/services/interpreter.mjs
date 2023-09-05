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

function interpretExpression(expression, eager = false) {
    let value, assignment = undefined;
    if (expression instanceof Basic) {
        value = expression.value;
        assignment = expression.assignment;
    }
    if(expression instanceof Value) {
        value = expression;
    }


    if (assignment) {
        if(eager) {
            memory.set(assignment.name, {
                expression: assignment.expr,
                value: interpretExpression((assignment.expr))
            })
        }else {
            memory.set(assignment.name, {
                expression: assignment.expr,
                value: undefined,
            })
        }
    }
    if (value) {
        if (value.number) {
            return value.number
        }
        if (value.name) {
            return findResult(value.name)
        }
        if (value.functionDefinition) {
            return value.functionDefinition;
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
                const names = result.names;
                const values = value.functionCall.values;
                const expressions = result.expressions;
                for (let i = 0; i < names.length; i++) {
                    memory.set(names[i], {
                        expression: values[i],
                        value: undefined,
                    });
                }
                for (let expression of expressions) {
                    interpretExpression(expression, true)
                }
                for (let i = 0; i < names.length; i++) {
                    if(values[i].name) {
                        memory.set(values[i].name, memory.get(names[i]));
                    }
                }
                return result
            }
        }
    }
}

function interpret(expressions) {
    for (let expression of expressions) {
        const result = interpretExpression(expression);
        if(!isNaN(result)){
            console.log(result);
        }
    }
}

const builtins = {
    "print": (value) => console.log(value),
    "plus": (values) => values[0] + values[1],
    "mult": (values) => values[0] * values[1],
    "div": (values) => values[0] / values[1],
    "modulo": (values) => values[0] % values[1],
}

export default interpret;
