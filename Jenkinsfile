pipeline {
  agent any
  stages {
    stage('Configure Cabal project') {
      steps {
        sh 'nix-shell . --run \'cabal new-configure --enable-tests\' --arg ci true'
      }
    }
    stage('Build Amulet library') {
      steps {
        sh 'nix-shell . --run \'cabal new-build -j2 --ghc-option=-Werror\' --arg ci true'
      }
    }
    stage('Run tests') {
      parallel {
        stage('Run tests') {
          steps {
            sh '''nix-shell . --run "cabal new-test --ghc-option=-Werror" --arg ci true
'''
          }
        }
        stage('Run hlint') {
          steps {
            sh '''nix-shell . --run "hlint --git" --arg ci true
'''
          }
        }
      }
    }
  }
}