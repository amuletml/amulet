pipeline {
  agent any

  environment {
    SIGNING_KEY_PW = credentials('jenkins-signing-key-pass')
  }

  stages {
    stage('Set up Stack dependencies') {
      steps {
        sh 'stack exec --package=hlint -- true'
      }
    }

    stage('Initial validation') {
      parallel {
        stage('Lint the pushed code') {
          steps {
            sh 'stack exec --package=hlint -- hlint -v --git'
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

    stage('Run the test suite') {
      steps {
        sh '''
          env AMC_LIBRARY_PATH=${PWD}/lib \
          stack test \
            --flag amuletml:amc-prove-server \
            --test-arguments "-j6 --xml junit.xml --display t --hedgehog-tests 5000 -v"
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

    aborted {
      slackSend color: '#808080',
        message: "Build ${env.BUILD_NUMBER} (Triggered by commit to ${env.BRANCH_NAME}) aborted",
        iconEmoji: 'unamused',
        username: "Amulet Jenkins"
    }

    failure {
      slackSend color: 'danger',
        message: "Build ${env.BUILD_NUMBER} (Triggered by commit to ${env.BRANCH_NAME}) failed :disappointed:",
        iconEmoji: 'unamused',
        username: "Amulet Jenkins"
    }

    success {
      slackSend color: 'good',
        message: "Build ${env.BUILD_NUMBER} (Triggered by commit to ${env.BRANCH_NAME}) passed! :tada:"

      sh 'tools/sign.sh'
      archiveArtifacts artifacts: 'result/*'

      sh '''
        cp result/*.pkg.tar* /srv/http/x86_64/
        repo-add -R -p /srv/http/x86_64/amuletml-nightly.db.tar.gz \
          /srv/http/x86_64/*.pkg.tar
      '''
    }
  }
}
