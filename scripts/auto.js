const { inspect } = require('util')

const execa = require('execa')

{
  execa('elm', ['init'], {
    input: 'Y\n',
    stdout: 'inherit',
    stderr: 'inherit',
  })
    .then(console.log)
    .catch(e => {
      setTimeout(() => {
        console.error('INSPECT ERROR', inspect(e, true, 100, true))
      }, 1000)
    })
}
