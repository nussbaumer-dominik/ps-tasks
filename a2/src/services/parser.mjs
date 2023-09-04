import {Type} from "../models/type.mjs";
import {Expr, Basic, Value, FunctionCall, Assign} from "../models/expr.mjs";

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
        //TODO get scoped expression
        const expression = [basicTokens[0]];
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

function isFunctioncall(valueTokens) {
    return valueTokens[0].type === Type.ENTITY && valueTokens[1]?.type === Type.LCURLY;
}

function parseValue(valueTokens) {
    let basic = null;
    //check for number
    if (valueTokens[0].type === Type.NUMBER) {
        basic = Value.createNumber(valueTokens[0].text);
        valueTokens.shift();
    }
    else if (isFunctioncall(valueTokens)) {
        basic = parseFunctionCall(valueTokens);
    }  else if (valueTokens[0].type === Type.ENTITY || valueTokens[0].type === Type.OPERATION) {
        //check for name
        basic = Value.createName(valueTokens[0].text);
        valueTokens.shift();
    }else {
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
        if (isFunctioncall(valueTokens)) {
            resultValues.push(parseFunctionCall(valueTokens));
        } else
        if ((token.type === Type.NUMBER || token.type === Type.ENTITY) && (next === undefined || next.type === Type.COMMA)) {
            resultValues.push(parseValue([token]))
            valueTokens.shift();
            valueTokens.shift();
        }
    }
    return resultValues;
}

export default parse;
