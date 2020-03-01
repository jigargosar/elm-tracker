const { inspect } = require('util')

const execa = require('execa')

{
  execa('elmelm', ['init'], {
    input: 'Y\n',
    stdout: 'inherit',
    stderr: 'inherit',
  })
    .then(console.log)
    .catch(e => {
      setTimeout(() => {
        console.error('ERROR: INSPECTING', inspect(e, false, 0, true))
      }, 1000)
    })
}