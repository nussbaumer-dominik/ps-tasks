"use strict";
const Token = require('./token');
const Type = require('./type');
require('./operations');

function lex(input) {
    console.log(`lexing input: ${input}`);
    if (input === undefined) {
        return [];
    }

    let tokens = [];
    //let inputSplit = input.split(' ');
    let currentPos = 0;

    while (currentPos < input.length) {
        console.log(`currentPos: ${currentPos}, ${input[currentPos]}`);
        let char = input[currentPos];
        //skip whitespace
        if (char === ' ') {
            currentPos++;
            continue;
        }
        if (char === '(') {
            tokens.push(new Token(Type.LPAREN, '(', currentPos));
            currentPos++;
            continue;
        }
        if (char === ')') {
            tokens.push(new Token(Type.RPAREN, ')', currentPos));
            currentPos++;
            continue;
        }
        //check for  "->"
        if (char === '-') {
            if (currentPos + 1 < input.length && input[currentPos + 1] === '>') {
                tokens.push(new Token(Type.LAMBDA, '->', currentPos));
                currentPos = currentPos + 2;
                continue;
            }
        }
        //check the word length
        let wordLength = 0;
        let word = input[currentPos];
        while (currentPos + wordLength <= input.length) {
            //check if the word ends
            if (input[currentPos + wordLength] === '(' || input[currentPos + wordLength] === ')' || input[currentPos + wordLength] === ' ' || currentPos + wordLength + 1 === input.length) {
                tokens.push(new Token(getType(word), word, currentPos));
                currentPos = wordLength + currentPos + 1;
                break;
            }
            wordLength++;
            word += input[currentPos + wordLength];
        }

    }
    tokens.push(new Token(Type.EOF, "<EOF>", input.length))
    return tokens
}

function getType(word) {
    if(word === "plus" || word === "minus" || word === "mult" || word === "div" || word === "cond") {
        return Type.OPERATION
    }
    //check if word is a number
    if (!isNaN(word)) {
        return Type.NUMBER;
    }
    return Type.ENTITY;
}


module.exports = {
    lex
};
