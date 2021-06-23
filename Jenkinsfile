pipeline {
  agent {
    dockerfile {
      label 'docker'
      additionalBuildArgs '--build-arg K_COMMIT=$(cd ext/k && git tag --points-at HEAD | cut --characters=2-) --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
    }
  }
  options { ansiColor('xterm') }
  environment { LONG_REV = """${sh(returnStdout: true, script: 'git rev-parse HEAD').trim()}""" }
  stages {
    stage('Init title') {
      when { changeRequest() }
      steps { script { currentBuild.displayName = "PR ${env.CHANGE_ID}: ${env.CHANGE_TITLE}" } }
    }
    stage('Build') {
      parallel {
        stage('K')      { steps { sh 'make build-k -j8 RELEASE=true' } }
      }
    }
    stage('Test') {
      options { timeout(time: 15, unit: 'MINUTES') }
      parallel {
        stage('Unit')             { steps { sh 'make test-unit     -j8' } }
        stage('Symbolic')         { steps { sh 'make test-symbolic -j2' } }
        stage('Prove')            { steps { sh 'make test-prove    -j2' } }
      }
    }
    stage('Integration Proofs') { 
      options { timeout(time: 180, unit: 'MINUTES') }
      stages {
        stage('Audit Proofs') { steps { sh 'make dexter-prove lqt-prove -j4' } }
      }
    }
    stage('Cross Test') {
      stages {
        stage('Build Tezos')      { steps { sh 'make deps-tezos'        } }
        stage('Build Compat')     { steps { sh 'make build-compat -j8 RELEASE=true' } }
        stage('Cross-Validation') { steps { sh 'make test-cross    -j8' } }
      }
    }
    stage('Deploy') {
      when { branch 'master' }
      stages {
        stage('Update Dependents') {
          steps {
            build job: 'rv-devops/master', propagate: false, wait: false                                                        \
                , parameters: [ booleanParam ( name: 'UPDATE_DEPS'         , value: true                                      ) \
                              , string       ( name: 'UPDATE_DEPS_REPO'    , value: 'runtimeverification/michelson-semantics' ) \
                              , string       ( name: 'UPDATE_DEPS_VERSION' , value: "${env.LONG_REV}")                          \
                              ]
          }
        }
        stage('GitHub Pages') {
          steps {
            sshagent(['2b3d8d6b-0855-4b59-864a-6b3ddf9c9d1a']) {
              dir('project-site') {
                sh '''
                  git clone 'ssh://github.com/runtimeverification/michelson-semantics.git'
                  cd michelson-semantics
                  git checkout -B gh-pages origin/master
                  git submodule update --init --recursive -- ./web
                  cd web
                  npm install
                  npm run build
                  npm run build-sitemap
                  cd -
                  mv web/public_content ./
                  rm -rf $(find . -maxdepth 1 -not -name public_content -a -not -name .git -a -not -path . -a -not -path .. -a -not -name CNAME)
                  mv public_content/* ./
                  rm -rf public_content
                  git add ./
                  git commit -m 'gh-pages: Updated the website'
                  git merge --strategy ours origin/gh-pages --allow-unrelated-histories
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
