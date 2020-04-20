pipeline {
  agent { dockerfile { label 'docker' } }
  options { ansiColor('xterm') }
  stages {
    stage("Init title") {
      when { changeRequest() }
      steps { script { currentBuild.displayName = "PR ${env.CHANGE_ID}: ${env.CHANGE_TITLE}" } }
    }
    stage('Build and Test') {
      when { changeRequest() }
      stages {
        stage('Dependencies') { steps { sh './build-deps.sh' } }
        stage('Build')        { steps { sh './build.sh'      } }
        stage('Test')         { steps { sh './run-tests.sh'  } }
      }
    }
    stage('Deploy') {
      when { branch 'master' }
      stages {
        stage('GitHub Pages') {
          steps {
            sshagent(['2b3d8d6b-0855-4b59-864a-6b3ddf9c9d1a']) {
              dir("kevm-${env.VERSION}-jello-paper") {
                sh '''
                  git clone 'ssh://github.com/runtimeverification/michelson-semantics.git'
                  cd evm-semantics
                  git checkout -B gh-pages origin/master
                  rm -rf build-coverage.sh build-deps.sh build.sh common.sh compat coverage.xml Dockerfile ext hex.cpp Jenkinsfile LICENSE media parser.sh run-coverage.sh run.sh run-tests.sh shutdown.sh start.sh tests time.cpp unit-test-kompiled
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
