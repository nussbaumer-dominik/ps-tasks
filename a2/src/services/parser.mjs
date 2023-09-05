import {Type} from "../models/type.mjs";
import {Expr, Basic, Value, FunctionCall, Assign, FunctionDefinition} from "../models/expr.mjs";

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

function parseBasic(basicTokens) {
    //check for lambda
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

    //else check for apply
    return Basic.createValue(parseValue(basicTokens));
}

function parseFunctionCall(valueTokens) {
    const name = valueTokens[0].text;
    //check for expr
    valueTokens.shift();
    valueTokens.shift();
    //check for closing parenthesis
    let i = 0;
    let curlyCount = 1;
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
    let exprTokens = valueTokens.splice(0, i - 1);
    //remove closing parenthesis
    valueTokens.shift();
    return Value.createFunctionCall(new FunctionCall(name, parseValues(exprTokens)));
}

function isFunctionCall(valueTokens) {
    return valueTokens[0].type === Type.ENTITY && valueTokens[1]?.type === Type.LCURLY;
}

function isFunctionDefinition(valueTokens) {
    return valueTokens[0].type === Type.LPAREN;
}

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
    valueTokens.shift(); // =>
    valueTokens.shift(); // {
    let i = 0;
    let curlyCount = 1;
    while (curlyCount > 0) {
        if (valueTokens[i].type === Type.RCURLY) {
            curlyCount--;
        }
        if (valueTokens[i].type === Type.LCURLY) {
            curlyCount++;
        }
        i++;
    }
    let exprTokens = valueTokens.splice(0, i - 1);
    const expressions = parse(exprTokens);
    //remove closing parenthesis
    valueTokens.shift(); // }
    return Value.createFunctionDefinition(new FunctionDefinition(names, expressions));
}

function parseValue(valueTokens) {
    let basic = null;
    //check for number
    if (valueTokens[0].type === Type.NUMBER) {
        basic = Value.createNumber(valueTokens[0].text);
        valueTokens.shift();
    } else if (isFunctionDefinition(valueTokens)) {
        basic = parseFunctionDefinition(valueTokens);
    } else if (isFunctionCall(valueTokens)) {
        basic = parseFunctionCall(valueTokens);
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


function parseValues(valueTokens) {
    const resultValues = [];
    while (valueTokens.length > 0) {
        const token = valueTokens[0];
        const next = valueTokens[1];
        if (isFunctionCall(valueTokens)) {
            resultValues.push(parseFunctionCall(valueTokens));
        } else if ((token.type === Type.NUMBER || token.type === Type.ENTITY) && (next === undefined || next.type === Type.COMMA || next.type === Type.RSQUARE)) {
            resultValues.push(parseValue([token]))
            valueTokens.shift();
            valueTokens.shift();
        }
    }
    return resultValues;
}

function parseList(listTokens) {
    //check for opening square bracket
    listTokens.shift();
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
