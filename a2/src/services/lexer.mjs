import {Token} from "../models/token.mjs";
import {Type} from "../models/type.mjs";


const wordBreaks = [' ', '(', ')', '{', '}', ',']

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
        if (char === '{') {
            tokens.push(new Token(Type.LCURLY, '{', currentPos));
            currentPos++;
            continue;
        }
        if (char === '}') {
            tokens.push(new Token(Type.RCURLY, '}', currentPos));
            currentPos++;
            continue;
        }
        if (char === ',') {
            tokens.push(new Token(Type.COMMA, ',', currentPos));
            currentPos++;
            continue;
        }
        //check for assignment :=
        if (char === ':' && currentPos + 1 < input.length  && input[currentPos + 1] === '=') {
            tokens.push(new Token(Type.ASSIGN, ':=', currentPos));
            currentPos = currentPos + 2;
            continue;
        }

        //check the word length
        let wordLength = 1;
        let word = input[currentPos];
        while (currentPos + wordLength <= input.length) {
            //check if the word ends
            const nextChar = input[currentPos + wordLength];
            if (wordBreaks.includes(nextChar) || currentPos+wordLength === input.length) {
                tokens.push(new Token(getType(word), word, currentPos));
                currentPos += wordLength;
                break;
            }
            word += input[currentPos + wordLength];
            wordLength++;
        }
    }
    tokens.push(new Token(Type.EOL, "<EOL>", input.length))
    return tokens
}

function getType(word) {
    if (word === "plus" || word === "minus" || word === "mult" || word === "div" || word === "cond") {
        return Type.ENTITY
    }
    //check if word is a number
    if (!isNaN(word)) {
        return Type.NUMBER;
    }
    return Type.ENTITY;
}


export default lex;
