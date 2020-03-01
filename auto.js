const spawn = require('child_process').spawn

spawn('elm', ['init'], { shell: true, stdio: 'inherit' })
