import {Basic, Value} from "../models/expr.mjs";

const memory = new Map()

function findResult(name) {
    if (!memory.has(name)) {
        return undefined;
    }
    const entry = memory.get(name);
    if (entry.value) {
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
    if (expression instanceof Value) {
        value = expression;
    }


    if (assignment) {
        if (eager) {
            memory.set(assignment.name, {
                expression: assignment.expr,
                value: interpretExpression((assignment.expr), eager)
            })
        } else {
            memory.set(assignment.name, {
                expression: assignment.expr,
                value: undefined,
            })
        }
    }
    if (value) {
        if (value.number !== undefined) {
            return value.number
        }
        if (value.list) {
            return value.list.map(entry => interpretExpression(entry, eager))
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
                    if(value.functionCall.name === "cond"){
                        return builtin(value.functionCall.values);
                    }
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
                    if (values[i].name) {
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
        if (!isNaN(result)) {
            console.log(result);
        }
    }
}

const builtins = {
    "print": (value) => {
        console.log(value)
        return null;
    },
    "plus": (values) => values[0] + values[1],
    "minus": (values) => values[0] - values[1],
    "mult": (values) => values[0] * values[1],
    "div": (values) => values[0] / values[1],
    "modulo": (values) => values[0] % values[1],
    "each": (values) => {
        values[0].forEach(value => {
            memory.set(values[1].names[0], {
                expression: undefined,
                value: value,
            });

            for (let expression of values[1].expressions) {
                interpretExpression(expression, true)
            }
        })
    },
    "cond": (values) => {
        const logicResult =  interpretExpression(values[0], true)
        if (logicResult === 1) {
            return interpretExpression(values[1], true)
        } else {
            return interpretExpression(values[2], true)
        }
    },
    "eq": (values) => values[0] === values[1] ? 1 : 0,
    "lt": (values) => values[0] < values[1] ? 1 : 0,
    "gt": (values) => values[0] > values[1] ? 1 : 0,
    "and": (values) => values[0] === 1 && values[1] === 1 ? 1 : 0,
    "or": (values) => values[0] === 1 || values[1] === 1 ? 1 : 0,
    "not": (value) => value === 0 ? 1 : 0,
}

export default interpret;
