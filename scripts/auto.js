const spawn = require('child_process').spawn

const execa = require('execa')

{
  execa('elm', ['init'], { input: 'Y\n', stdout: 'inherit' })
    .then(console.log)
    .catch(console.error)
}
