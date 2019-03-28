pipeline {
  agent any
  stages {
    stage('Build documentation') {
      steps {
        timestamps() {
          sh 'pdflatex doc/tc.tex'
        }
      }
    }
    stage('Build Amulet') {
      steps {
        timestamps() {
          sh 'stack build --fast'
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
        stage('Check code style') {
          steps {
            sh 'stack exec -- hlint --git'
          }
        }
      }
    }
  }
  post {
    always {
      archiveArtifacts artifacts: '*.pdf'
      junit 'junit.xml'
    }
  }
}
