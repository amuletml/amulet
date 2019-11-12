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

    stage('Compile the project') {
      steps {
        sh '''
          stack build \
            --ghc-options "-optc-static -optl-static -fhide-source-paths" \
            --flag amuletml:amc-prove-server \
            --test
        '''
      }
    }

    stage('Run tests') {
      steps {
        sh '''
          env AMC_LIBRARY_PATH=${PWD}/lib \
          stack test \
          --test-arguments "-j6 --xml junit.xml --display t --hedgehog-tests 5000"
        '''
      }
    }

    stage('Compile for deployment') {
      steps {
        sh './deploy-build.sh'
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
