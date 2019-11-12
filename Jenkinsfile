pipeline {
  agent any

  environment {
    SIGNING_KEY_PW = credentials('jenkins-signing-key-pass')
  }

  stages {
    stage('Set up stack') {
      steps {
        sh 'stack exec --package=hlint -- true'
      }
    }

    stage('') {
      parallel {
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
                --test --no-run-tests -j6
            '''
          }
        }
      }
    }

    stage('Run tests') {
      steps {
        sh '''
          env AMC_LIBRARY_PATH=${PWD}/lib \
          stack test \
            --flag amuletml:amc-prove-server \
            --test-arguments "-j6 --xml junit.xml --display t --hedgehog-tests 5000"
        '''
      }
    }

    stage('Compile for deployment') {
      steps {
        sh 'tools/deploy-build.sh'
      }
    }
  }

  post {
    always {
      junit 'junit.xml'
    }

    success {
      sh 'tools/sign.sh'

      archiveArtifacts artifacts: 'result/*'
    }
  }
}
