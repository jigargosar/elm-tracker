const { inspect } = require('util')
const DEBUG = false
const execa = require('execa')

;(async () => {
  try {
    await execa('elm', ['init'], {
      input: 'Y\n',
      stdout: 'inherit',
      stderr: 'inherit',
    })
  } catch (e) {
    if (DEBUG) {
      console.error('ERROR: INSPECTING', inspect(e, false, 0, true))
    } else {
      console.error('ERROR: ', e.message, e.code)
    }
  }

  await execa('elm', ['install', 'elm/core'], {
    input: 'Y\n',
    stdout: 'inherit',
    stderr: 'inherit',
  })
})()
