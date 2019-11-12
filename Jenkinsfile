pipeline {
  agent any

  environment {
    AMC_LIBRARY_PATH = "${PWD}/lib/"
  }

  stages {
    stage('Validate the pushed code') {
      steps {
        sh 'stack exec --package=hlint -- hlint --git'
      }
    }
    stage('Run tests') {
      steps {
        sh 'stack test --fast --test-arguments "--xml junit.xml --display t --hedgehog-tests 10000"'
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
