import {Basic, Value} from "../models/expressionClasses.mjs";

const memory = new Map()

/**
 * Interprets a list of basic expressions
 * @param expressions - A list of basic expressions
 */
function interpret(expressions) {
    for (let expression of expressions) {
        const result = interpretBasic(expression);
        if (!isNaN(result)) {
            console.log(result);
        }
    }
}

/**
 * Finds the result of a name/variable that is stored in the memory
 * @param name - The name of the variable
 * @returns {*|undefined}
 */
function findResult(name) {
    if (!memory.has(name)) {
        return undefined;
    }
    const entry = memory.get(name);
    if (entry.value) {
        return entry.value;
    }
    return interpretBasic(entry.expression);
}

/**
 * Interprets a basic expression
 * @param basicExpr - A basic expression
 * @param eager
 * @returns {*}
 */
function interpretBasic(basicExpr, eager = false) {
    let value, assignment = undefined;
    if (basicExpr instanceof Basic) {
        value = basicExpr.value;
        assignment = basicExpr.assignment;
    }
    if (basicExpr instanceof Value) {
        value = basicExpr;
    }

    if (assignment) {
        interpretAssignment(eager, assignment);
    }
    if (value) {
        return interpretValue(value, eager);
    }
}

function interpretAssignment(eager, assignment) {
    if (eager) {
        memory.set(assignment.name, {
            expression: assignment.expr,
            value: interpretBasic((assignment.expr), eager)
        })
    } else {
        memory.set(assignment.name, {
            expression: assignment.expr,
            value: undefined,
        })
    }
}

function interpretFunctionCall(value) {
    const result = findResult(value.functionCall.name)
    if (result === undefined) {
        const builtin = builtins[value.functionCall.name];
        //check if it is a builtin function
        if (builtin) {
            if (value.functionCall.name === "cond") {
                return builtin(value.functionCall.values);
            }
            const callValues = value.functionCall.values.map(interpretBasic)
            return builtin(callValues)
        } else {
            throw new Error(`Function ${value.functionCall.name} not found`)
        }
    } else {
        //since it's not a builtin function, it must be a function definition
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
            interpretBasic(expression, true)
        }
        for (let i = 0; i < names.length; i++) {
            if (values[i].name) {
                memory.set(values[i].name, memory.get(names[i]));
            }
        }
        return result
    }
}

function interpretValue(value, eager) {
    if (value.number !== undefined) {
        return value.number
    }
    if (value.list) {
        return value.list.map(entry => interpretValue(entry, eager))
    }
    if (value.name) {
        return findResult(value.name)
    }
    if (value.functionDefinition) {
        return value.functionDefinition;
    }
    if (value.functionCall) {
        return interpretFunctionCall(value);
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
                interpretBasic(expression, true)
            }
        })
    },
    "cond": (values) => {
        const logicResult = interpretBasic(values[0], true)
        if (logicResult === 1) {
            return interpretBasic(values[1], true)
        } else {
            return interpretBasic(values[2], true)
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
