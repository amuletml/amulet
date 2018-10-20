pipeline {
  agent any
  stages {
    stage('Build documentation') {
      steps {
        sh 'nix-shell . --run true' /* fetch Nix dependencies first */
        timestamps() {
          sh 'pdflatex doc/tc.tex'
        }
      }
    }
    stage('Configure Cabal project') {
      steps {
        sh 'nix-shell . --run \'cabal new-configure --enable-tests --disable-profiling --ghc-options="-Werror -fhide-source-paths"\' --arg ci true'
      }
    }
    stage('Build Amulet') {
      steps {
        timestamps() {
          sh 'nix-shell . --run \'cabal new-build -j6 test:tests\' --arg ci true'
        }
      }
    }
    stage('Run tests') {
      parallel {
        stage('Run tests') {
          steps {
            timestamps () {
              sh 'nix-shell . --run "cabal new-run test:tests -- --xml junit.xml " --arg ci true'
            }
          }
        }
        stage('Check code style') {
          steps {
            sh 'nix-shell . --run \'cabal new-build -j6 exe:amc\' --arg ci true'
            sh 'nix-shell . --run "hlint --git" --arg ci true'
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
