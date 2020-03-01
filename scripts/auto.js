const { inspect } = require('util')
const DEBUG = false
const execa = require('execa')
const pSeries = require('p-series')

;(async () => {
  try {
    await execa('elm', ['init'], {
      input: 'Y\n',
      stdout: DEBUG ? 'inherit' : 'ignore',
      stderr: DEBUG ? 'inherit' : 'ignore',
    })
  } catch (e) {
    if (DEBUG) {
      console.error('ERROR: INSPECTING', inspect(e, false, 0, true))
    } else if (e.code !== 1) {
      console.error('ERROR: ', e.message, e.code)
    }
  }

  const packageNames = [
    //
    'elm/svg',
    'elm/json',
    'elm/random',
    'elm/time',
    'elm-community/list-extra',
    'elm-community/basics-extra',
    'elm-community/html-extra',
    'laserpants/elm-update-pipeline',
  ]
  await pSeries(packageNames.map(name => () => installPackage(name)))
})()

function installPackage(packageName) {
  console.log('Installing: ' + packageName)
  return execa('elm', ['install', packageName], {
    input: 'Y\n',
    stdout: DEBUG ? 'inherit' : null,
    stderr: 'inherit',
  })
}
