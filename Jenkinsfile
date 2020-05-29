pipeline {
  agent {
    dockerfile {
      label 'docker'
      additionalBuildArgs '--build-arg K_COMMIT=$(cd ext/k && git rev-parse --short=7 HEAD) --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
    }
  }
  options { ansiColor('xterm') }
  stages {
    stage('Init title') {
      when { changeRequest() }
      steps { script { currentBuild.displayName = "PR ${env.CHANGE_ID}: ${env.CHANGE_TITLE}" } }
    }
    stage('Build and Test') {
      when { changeRequest() }
      stages {
        stage('Tezos Dependencies')    { steps { sh 'make deps-tezos'             } }
        stage('Build')                 { steps { sh 'make build -j8 RELEASE=true' } }
        stage('Build Compat')          { steps { sh './compat/build.sh'           } }
        stage('Test')                  { steps { sh './run-tests.sh'              } }
        stage('Cross-Validation Test') { steps { sh './compat/run-tests-ci.sh'    } }
      }
    }
    stage('Deploy') {
      when { branch 'master' }
      stages {
        stage('GitHub Pages') {
          steps {
            sshagent(['2b3d8d6b-0855-4b59-864a-6b3ddf9c9d1a']) {
              dir('project-site') {
                sh '''
                  git clone 'ssh://github.com/runtimeverification/michelson-semantics.git'
                  cd michelson-semantics
                  git checkout -B gh-pages origin/master
                  # delete all non-markdown files EXCEPT the _config.yml file which defines the website theme
                  # note: since we are in a groovy string, we need to escape each backslash
                  rm -r $(git ls-files | grep -v -E '\\.md$|^_config\\.yml$')
                  # delete media directory which we don't care about
                  rm -rf media
                  git add ./
                  git commit -m 'gh-pages: remove unrelated content'
                  git fetch origin gh-pages
                  git merge --strategy ours FETCH_HEAD
                  git push origin gh-pages
                '''
              }
            }
          }
        }
      }
    }
  }
}
