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
      // console.error('ERROR: INSPECTING', inspect(e, false, 0, true))
      console.error('ERROR: ', e.message)
    })
}
