pipeline {
  agent any
  stages {
    stage('Validate the pushed code') {
      parallel {
        stage('Load Amulet into GHCi, once') {
          steps {
            sh "echo ':q' | ./repl.sh -fno-code"
          }
        }
        stage('Check code style') {
          steps {
            sh 'stack exec -- hlint --git'
          }
        }
      }
    }
    stage('Run tests') {
      parallel {
        stage('Run tests') {
          steps {
            timestamps () {
              sh 'stack test --fast --test-arguments "--xml junit.xml --display t"'
            }
          }
        }
      }
    }
    stage('Compile for deployment') {
      steps {
        timestamps () {
          sh './deploy-build.sh'
        }
      }
    }
  }
  post {
    always {
      archiveArtifacts artifacts: 'result/*'
      junit 'junit.xml'
    }
  }
}
