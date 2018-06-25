pipeline {
  agent any
  stages {
    stage('Configure Cabal project') {
      parallel {
        stage('Configure Cabal project') {
          steps {
            sh 'nix-shell . --run \'cabal new-configure --enable-tests --disable-profiling --ghc-options="-Werror -fhide-source-paths"\' --arg ci true'
          }
        }
        stage('Run hlint') {
          steps {
            timestamps() {
              sh 'nix-shell . --run "hlint --git" --arg ci true'
            }

          }
        }
      }
    }
    stage('Build Amulet') {
      steps {
        sh 'nix-shell . --run \'cabal new-build -j2 test:tests\' --arg ci true'
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
        stage('Build Compiler') {
          steps {
            sh 'nix-shell . --run \'cabal new-build -j2 exe:amc\' --arg ci true'
          }
        }
      }
    }
  }
}