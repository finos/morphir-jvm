# Creating a new Release

The release of this repo is automated through GitHub actions. To trigger a new release you have to use the GitHub Releases page to create a new release. Here's how that's done step-by-step:

1. Go to the Releases tab under the `finos/morphir-jvm` repo. Click on "Draft a new release": ![screenshot1](assets/release-step-1.jpg)
2. Click on the "Choose a tag" dropdown to find the latest release: ![screenshot2](assets/release-step-2.jpg)
3. Come up with the next version number based on the changes that were added: ![screenshot3](assets/release-step-3.jpg)
4. Add release details and click "Publish release": ![screenshot4](assets/release-step-4.jpg)
5. GitHub Action will automatically be triggered. When the job completes it will be published to Maven: ![screenshot5](assets/release-step-5.jpg)
