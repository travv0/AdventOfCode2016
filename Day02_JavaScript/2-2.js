var fs = require('fs');

function moveUp(n) {
    if (n === '3') return '1';
    else if (n === '6') return '2';
    else if (n === '7') return '3';
    else if (n === '8') return '4';
    else if (n === 'A') return '6';
    else if (n === 'B') return '7';
    else if (n === 'C') return '8';
    else if (n === 'D') return 'B';
    else return n;
}

function moveDown(n) {
    if (n === '1') return '3';
    else if (n === '2') return '6';
    else if (n === '3') return '7';
    else if (n === '4') return '8';
    else if (n === '6') return 'A';
    else if (n === '7') return 'B';
    else if (n === '8') return 'C';
    else if (n === 'B') return 'D';
    else return n;
}

function moveRight(n) {
    if (n === '2') return '3';
    else if (n === '3') return '4';
    else if (n === '5') return '6';
    else if (n === '6') return '7';
    else if (n === '7') return '8';
    else if (n === '8') return '9';
    else if (n === 'A') return 'B';
    else if (n === 'B') return 'C';
    else return n;
}

function moveLeft(n) {
    if (n === '3') return '2';
    else if (n === '4') return '3';
    else if (n === '6') return '5';
    else if (n === '7') return '6';
    else if (n === '8') return '7';
    else if (n === '9') return '8';
    else if (n === 'B') return 'A';
    else if (n === 'C') return 'B';
    else return n;
}

function getNumber(n, commands) {
    var num = n;
    for (var i = 0, len = commands.length; i < len; i++) {
        var command = commands.charAt(i);
        if (command == 'U') num = moveUp(num);
        else if (command == 'D') num = moveDown(num);
        else if (command == 'R') num = moveRight(num);
        else if (command == 'L') num = moveLeft(num);
    }
    return num;
}

function readInstructions(instructions) {
    var code = "";
    var num = '5';
    for (var i = 0, len = instructions.length; i < len; i++) {
        num = getNumber(num, instructions[i]);
        code += num;
    }
    return code;
}

function solveCodeFromInput(input) {
    var lines = input.match(/[^\r\n]+/g);
    return readInstructions(lines);
}

fs.readFile('input.txt', 'utf8', function(err, input) {
    if (err)
        console.log('Could not read input file.');
    else
        console.log(solveCodeFromInput(input));
});
