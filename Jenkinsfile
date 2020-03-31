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
      agent {
      }
      stages {
        stage('Dependencies') { steps { sh 'make deps RELEASE=true' } }
        stage('Build')        { steps { sh 'make build -j4'         } }
        stage('Build')        { steps { sh 'make test -j4'          } }
    }
    // stage('Deploy') {
    //   when { branch 'master' }
    //     stage('GitHub Pages') {
    //       steps {
    //         sshagent(['2b3d8d6b-0855-4b59-864a-6b3ddf9c9d1a']) {
    //           dir("kevm-${env.VERSION}-jello-paper") {
    //             sh '''
    //               git config --global user.email "admin@runtimeverification.com"
    //               git config --global user.name  "RV Jenkins"
    //               mkdir -p ~/.ssh
    //               echo 'host github.com'                       > ~/.ssh/config
    //               echo '    hostname github.com'              >> ~/.ssh/config
    //               echo '    user git'                         >> ~/.ssh/config
    //               echo '    identityagent SSH_AUTH_SOCK'      >> ~/.ssh/config
    //               echo '    stricthostkeychecking accept-new' >> ~/.ssh/config
    //               chmod go-rwx -R ~/.ssh
    //               ssh github.com || true
    //               git clone 'ssh://github.com/runtimeverification/michelson-semantics.git'
    //               cd evm-semantics
    //               git checkout -B gh-pages origin/master
    //               rm -rf build-coverage.sh build.sh common.sh compat coverage.xml ext Jenkinsfile parser.sh run-coverage.sh run.sh run-tests.sh shutdown.sh start.sh tests time.cpp tolowercase.cpp
    //               git add ./
    //               git commit -m 'gh-pages: remove unrelated content'
    //               git fetch origin gh-pages
    //               git merge --strategy ours FETCH_HEAD
    //               git push origin gh-pages
    //             '''
    //           }
    //         }
    //       }
    //     }
    //   }
    // }
  }
}
