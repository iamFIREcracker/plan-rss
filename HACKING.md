# Release

- update the version inside 'plan-rss.asd'.
- update changelog
- `git commit ..` and `git push`
- wait for [travis-ci](https://travis-ci.org/iamFIREcracker/plan-rss) to finish the
  build
- `git tag $TAG` and `git push origin $TAG`
