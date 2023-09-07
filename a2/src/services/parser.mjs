import {Type} from "../models/type.mjs";
import {Basic, Value, FunctionCall, Assign, FunctionDefinition} from "../models/expr.mjs";

/**
 * Parses a list of tokens into a list of expressions
 * Generate the AST tree
 * @param tokens
 * @returns {*[]} - A list of basic expressions
 */
function parse(tokens) {
    const result = [];
    while (tokens.length > 0) {
        if (tokens[0].type === Type.EOL) {
            tokens.shift();
            continue;
        }
        result.push(parseBasic(tokens))
    }
    return result;
}

/**
 * Parses a basic expression
 * A basic expression is either a value or an assignment
 * @param basicTokens - A list of tokens representing a basic expression
 * @returns {Basic} - A basic expression
 */
function parseBasic(basicTokens) {
    //check for lambda - create basic with assignment
    if (basicTokens[0].type === Type.ENTITY && basicTokens.length > 1 && basicTokens[1].type === Type.ASSIGN) {
        let name = basicTokens[0].text;
        basicTokens.shift();
        basicTokens.shift();

        let minExpressionEndIndex = basicTokens.findIndex(t => t.type === Type.EOL)
        if (minExpressionEndIndex === -1) {
            minExpressionEndIndex = basicTokens.length -1;
        }
        let openParenthesis = 0;
        for (let i = 0; i < minExpressionEndIndex; i++) {
            if (basicTokens[i].type === Type.LCURLY) {
                openParenthesis++
            }
            if (basicTokens[i].type === Type.RCURLY) {
                openParenthesis--
            }
        }
        let endIndex = minExpressionEndIndex;
        while (openParenthesis > 0) {
            if (basicTokens[endIndex].type === Type.LCURLY) {
                openParenthesis++
            }
            if (basicTokens[endIndex].type === Type.RCURLY) {
                openParenthesis--
            }
            endIndex++;
        }

        const expression = basicTokens.splice(0, endIndex);
        basicTokens.shift();
        return Basic.createAssignment(new Assign(name, parseBasic(expression)));
    }

    //create basic with value
    return Basic.createValue(parseValue(basicTokens));
}

/**
 * Parses a function call
 * @param tokens - A list of tokens representing a function call
 * @returns {FunctionCall} - A function call
 */
function parseFunctionCall(tokens) {
    const name = tokens[0].text;

    let valueTokens = getTokensInBrackets(tokens);
    //remove closing parenthesis
    tokens.shift(); // }
    return new FunctionCall(name, parseValues(valueTokens));
}

/**
 * Checks if the tokens represent a function call
 * @param valueTokens - A list of tokens representing a function call
 * @returns {boolean}
 */
function isFunctionCall(valueTokens) {
    return valueTokens[0].type === Type.ENTITY && valueTokens[1]?.type === Type.LCURLY;
}

/**
 * Checks if the tokens represent a function definition
 * @param valueTokens - A list of tokens representing a function definition
 * @returns {boolean}
 */
function isFunctionDefinition(valueTokens) {
    return valueTokens[0].type === Type.LPAREN;
}

/**
 * Returns the tokens between the opening and closing parenthesis
 * Removes the opening and closing parenthesis
 * Changes the valueTokens list so that it only contains the tokens after the closing parenthesis
 * @param valueTokens - A list of tokens representing an expression
 * @returns {any[]} - A list of tokens that are between the opening and closing parenthesis
 */
function getTokensInBrackets(valueTokens) {
    valueTokens.shift(); // =>
    valueTokens.shift(); // {
    let i = 0;
    let curlyCount = 1;
    //check for closing parenthesis
    while (curlyCount > 0) {
        if (valueTokens[i].type === Type.RCURLY) {
            curlyCount--;
        }
        if (valueTokens[i].type === Type.LCURLY) {
            curlyCount++;
        }
        i++;
    }
    //split tokens at i
    return valueTokens.splice(0, i - 1);
}

/**
 * Parses a function definition
 * @param valueTokens - A list of tokens representing a function definition
 * @returns {FunctionDefinition} - A function definition
 */
function parseFunctionDefinition(valueTokens) {
    // Parse names
    valueTokens.shift(); // (
    //check for closing parenthesis
    const names = [];
    while (valueTokens[0].type !== Type.RPAREN) {
        if (valueTokens[0].type === Type.ENTITY) {
            names.push(valueTokens[0].text)
        }
        valueTokens.shift();
    }
    valueTokens.shift();

    // Parse function body
    let exprTokens = getTokensInBrackets(valueTokens);
    const expressions = parse(exprTokens);
    //remove closing parenthesis
    valueTokens.shift(); // }
    return new FunctionDefinition(names, expressions);
}

/**
 * Parses a value
 * @param valueTokens - A list of tokens representing a value
 * @returns {Value} - A value
 */
function parseValue(valueTokens) {
    let basic;
    //check for number
    if (valueTokens[0].type === Type.NUMBER) {
        basic = Value.createNumber(valueTokens[0].text);
        valueTokens.shift();
    } else if (isFunctionDefinition(valueTokens)) {
        basic = Value.createFunctionDefinition(parseFunctionDefinition(valueTokens));
    } else if (isFunctionCall(valueTokens)) {
        basic = Value.createFunctionCall(parseFunctionCall(valueTokens));
    } else if (valueTokens[0].type === Type.LSQUARE) {
        basic = Value.createList(parseList(valueTokens));
    } else if (valueTokens[0].type === Type.ENTITY || valueTokens[0].type === Type.OPERATION) {
        //check for name
        basic = Value.createName(valueTokens[0].text);
        valueTokens.shift();
    } else {
        //TODO validation
        valueTokens.shift();
    }
    return basic;
}

/**
 * Parses a list of values
 * @param valueTokens - A list of tokens representing a list of values
 * @returns {*[]} - A list of values
 */
function parseValues(valueTokens) {
    const resultValues = [];
    while (valueTokens.length > 0) {
        const token = valueTokens[0];
        const next = valueTokens[1];
        if (isFunctionCall(valueTokens)) {
            resultValues.push(Value.createFunctionCall(parseFunctionCall(valueTokens)));
        } else if ((token.type === Type.NUMBER || token.type === Type.ENTITY) && (next === undefined || next.type === Type.COMMA || next.type === Type.RSQUARE)) {
            resultValues.push(parseValue([token]))
            valueTokens.shift();
        }
        valueTokens.shift();
    }
    return resultValues;
}

/**
 * Parses a list of values
 * Lists must start with a left square bracket and end with a right square bracket
 * @param listTokens - A list of tokens representing a list of values
 * @returns {*[]} - A list of values
 */
function parseList(listTokens) {
    //check for opening square bracket
    if (listTokens[0].type !== Type.LSQUARE) {
        throw new Error("Invalid list");
    }
    listTokens.shift(); // [
    const values = [];
    while (listTokens[0].type !== Type.RSQUARE) {
        if (listTokens[0].type === Type.COMMA) {
            listTokens.shift();
            continue;
        }
        values.push(parseValue(listTokens));
    }
    listTokens.shift(); // [
    return values;
}

export default parse;
