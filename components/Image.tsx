import NextImage, { ImageProps } from 'next/image'

// @ts-ignore
const Image = ({ ...rest }: ImageProps) => <img {...rest} />

export default Image
