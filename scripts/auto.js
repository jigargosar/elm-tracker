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
    } else if (e.code !== 1) {
      console.error('ERROR: ', e.message, e.code)
    }
  }
  await installPackage('elm/core')
})()

function installPackage(packageName) {
  console.log('Installing: ' + packageName)
  return execa('elm', ['install', packageName], {
    input: 'Y\n',
    stdout: 'inherit',
    stderr: 'inherit',
  })
}
