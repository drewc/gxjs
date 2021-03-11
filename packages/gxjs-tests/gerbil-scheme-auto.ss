(import :js)
(js#declaration #<<EOF
console.log('Ok, auto RTS!', Object.keys(RTS.glo).length)

module.exports = RTS;

EOF
)
