let password = "heslicko";
let crypto = require('crypto');

function passwordHash(word, number) {
    let mixPassword = crypto.createHash('md5').update(word).digest('hex');
    return mixPassword;
}

console.log(passwordHash(password));