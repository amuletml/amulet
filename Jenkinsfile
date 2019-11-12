pipeline {
  agent any

  stages {
    stage('Set up stack') {
      steps {
        sh 'stack exec --package=hlint -- true'
      }
    }

    stage('Validate the pushed code') {
      steps {
        sh 'stack exec --package=hlint -- hlint --git'
      }
    }

    stage('Run tests') {
      steps {
        sh '''
          env AMC_LIBRARY_PATH=${PWD}/lib \
          stack test --fast \
          --test-arguments "--xml junit.xml --display t --hedgehog-tests 5000"
        '''
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
