import { useContext } from 'react'
import { I18nContext } from '../contexts/I18nContext'
export default function useTranslationFunction() {
return useContext(I18nContext)
}