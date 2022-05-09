#!groovy
pipeline {
    agent {
        label 'builder'
    }

    environment {
        // image details
        DEPLOYMENT_REGISTRY = 'registry.sireto.io'
        DEPLOYMENT_IMAGE_NAME = 'kuber/api'

        //From develop branch: deployment details for develop
        DEPLOYMENT_DEVELOP_CLOUD = 'cnftregistry.io:2376'
        DEPLOYMENT_DEVELOP_SERVICE = 'kuber-dev_api'

        // From master branch:deployment details for staging

        DEPLOYMENT_STAGING_CLOUD = 'cnftregistry.io:2376'
        DEPLOYMENT_STAGING_SERVICE = 'kuber-staging_api'

        //When tag is pushed: deployment details for release
        DEPLOYMENT_RELEASE_CLOUD = 'cnftregistry.io:2376'
        DEPLOYMENT_RELEASE_SERVICE = 'kuber_api'


        COMPUTED_PR = "${env.gitlabMergeRequestIid ? 'true' : ''}"
        COMPUTED_BRANCH = "${env.gitlabMergeRequestIid != null ? "origin/merge-requests/${env.gitlabMergeRequestIid}" : (env.gitlabBranch == null ? (env.GIT_BRANCH == null ? "origin/master" : env.GIT_BRANCH) : (env.gitlabBranch.contains("refs/") ? env.gitlabBranch : "origin/${env.gitlabBranch}"))}"
        COMPUTED_BRANCH_NAME = "${env.gitlabMergeRequestIid != null ? "${env.gitlabSourceNamespace}/${env.gitlabSourceBranch}" : (env.gitlabBranch == null ? (env.GIT_BRANCH == null ? "master" : env.GIT_BRANCH.split("/").last()) : env.gitlabBranch.contains("tags/v") ? env.gitlabBranch.split("/").last() : env.gitlabBranch)}"
        COMPUTED_TAG = "${"${env.gitlabBranch}".contains("tags/v") ? "${env.gitlabBranch}".split('/').last() : (env.gitlabMergeRequestIid != null ? '' : env.COMPUTED_BRANCH_NAME)}"
        COMPUTED_DOCKER_TAG = "${env.COMPUTED_TAG ? (env.COMPUTED_BRANCH_NAME == 'master' ? "latest-${env.BUILD_ID}" : (env.COMPUTED_BRANCH_NAME == "develop" ? "nightly-${env.BUILD_ID}" : env.COMPUTED_BRANCH_NAME == "release" ? "release-${env.BUILD_ID}" : '')) : ''}"
        COMPUTED_DOCKER_TAG_COMMON = "${env.gitlabMergeRequestIid != null ? '' : (env.COMPUTED_BRANCH_NAME == 'master' ? "latest" : (env.COMPUTED_BRANCH_NAME == "develop" ? "nightly" : (env.COMPUTED_BRANCH_NAME == "release" ? "release" : '')))}"
        DEPLOYMENT_CLOUD = "${env.COMPUTED_DOCKER_TAG_COMMON == "release" ? env.DEPLOYMENT_RELEASE_CLOUD : (env.COMPUTED_DOCKER_TAG_COMMON == "latest" ? env.DEPLOYMENT_STAGING_CLOUD : (env.COMPUTED_DOCKER_TAG_COMMON == 'nightly' ? (env.DEPLOYMENT_DEVELOP_CLOUD ? env.DEPLOYMENT_DEVELOP_CLOUD : '') : ''))}"
        DEPLOYMENT_SERVICE = "${env.COMPUTED_DOCKER_TAG_COMMON == "release" ? env.DEPLOYMENT_RELEASE_SERVICE : (env.COMPUTED_DOCKER_TAG_COMMON == "latest" ? env.DEPLOYMENT_STAGING_SERVICE : (env.COMPUTED_DOCKER_TAG_COMMON == 'nightly' ? (env.DEPLOYMENT_DEVELOP_SERVICE ? env.DEPLOYMENT_DEVELOP_SERVICE : '') : ''))}"
        BASE_PATH = "${ (env.COMPUTED_BRANCH_NAME == "release" || env.COMPUTED_BRANCH_NAME == "master" ) ? "/bazaar" : "" }"
        DEPLOYMENT_ENVIRONMENT = "${(env.COMPUTED_BRANCH_NAME == "release" || env.COMPUTED_BRANCH_NAME == "master" ) ? "production":"development"}"
    }
    stages {
        stage('Docker Build and Publish') {
            when {
                expression { env.COMPUTED_DOCKER_TAG != null }
            }
            steps {
                    script {
                        sh 'printenv'

                        docker.withRegistry("https://${DEPLOYMENT_REGISTRY}", "${DEPLOYMENT_REGISTRY}") {
                                sh "./.ci/build  -t ${DEPLOYMENT_REGISTRY}/${DEPLOYMENT_IMAGE_NAME}:${env.COMPUTED_DOCKER_TAG} -t ${DEPLOYMENT_REGISTRY}/${DEPLOYMENT_IMAGE_NAME}:${env.COMPUTED_DOCKER_TAG_COMMON} "
                                sh "docker push ${DEPLOYMENT_REGISTRY}/${DEPLOYMENT_IMAGE_NAME}:${env.COMPUTED_DOCKER_TAG}"
                                sh "docker push ${DEPLOYMENT_REGISTRY}/${DEPLOYMENT_IMAGE_NAME}:${env.COMPUTED_DOCKER_TAG_COMMON}"               
                            }

                    }
                    
            }
        }

        stage("Deploy") {
            when {
                expression {
                    env.COMPUTED_DOCKER_TAG_COMMON != null && env.DEPLOYMENT_SERVICE != null && env.DEPLOYMENT_CLOUD != null
                }
            }
            steps {
                    script {
                          docker.withServer("${DEPLOYMENT_CLOUD}") {
                              docker.withRegistry("https://${DEPLOYMENT_REGISTRY}", "${DEPLOYMENT_REGISTRY}") {
                                sh "docker service update --with-registry-auth --image ${DEPLOYMENT_REGISTRY}/${DEPLOYMENT_IMAGE_NAME}:${COMPUTED_DOCKER_TAG} ${DEPLOYMENT_SERVICE}"
                              }
                          }
                    }
            }
        }
    }
    post {
        unsuccessful {
            step([$class                  : 'Mailer',
                  notifyEveryUnstableBuild: true,
                  recipients              : [emailextrecipients([[$class: 'CulpritsRecipientProvider'], [$class: 'RequesterRecipientProvider']])].join(' ')])
        }

        success {
            updateGitlabCommitStatus name: 'Jenkins', state: 'success'
            // acceptGitLabMR(useMRDescription: true, removeSourceBranch: true)
        }
    }
}