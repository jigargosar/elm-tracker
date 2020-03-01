const { inspect } = require('util')
const DEBUG = false
const execa = require('execa')

{
  execa('elm', ['init'], {
    input: 'Y\n',
    stdout: 'inherit',
    stderr: 'inherit',
  })
    .then(console.log)
    .catch(e => {
      // noinspection PointlessBooleanExpressionJS,BadExpressionStatementJS
      if (DEBUG) {
        console.error('ERROR: INSPECTING', inspect(e, false, 0, true))
      } else {
        console.error('ERROR: ', e.message)
      }
    })
}
