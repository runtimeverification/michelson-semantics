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
    stage('Build') {
      parallel {
        stage('Tezos')  { steps { sh 'KNINJA_USE_SYSTEM_K=true ./build deps-tezos'       } }
        stage('K')      { steps { sh 'KNINJA_USE_SYSTEM_K=true ./build build-k      -j8' } }
        stage('Compat') { steps { sh 'KNINJA_USE_SYSTEM_K=true ./build build-compat -j8' } }
      }
    }
    stage('Test') {
      options { timeout(time: 10, unit: 'MINUTES') }
      parallel {
        stage('Unit')             { steps { sh 'KNINJA_USE_SYSTEM_K=true ./build test-unit     -j8' } }
        stage('Symbolic')         { steps { sh 'KNINJA_USE_SYSTEM_K=true ./build test-symbolic -j2' } }
        stage('Prove')            { steps { sh 'KNINJA_USE_SYSTEM_K=true ./build test-prove    -j2' } }
        stage('Cross-Validation') { steps { sh 'KNINJA_USE_SYSTEM_K=true ./build test-cross    -j8' } }
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
